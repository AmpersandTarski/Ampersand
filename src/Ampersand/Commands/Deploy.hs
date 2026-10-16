{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate what is needed to run a system of contexts.
--
--   Every database of a system has one application: that of the context that includes the other contexts on it.
--   The command @ampersand deploy@ writes, for the database of the context in the root file and every database
--   that it reaches: a Dockerfile that builds the application of that database on the prototype framework,
--   and one compose file that runs the applications next to one database server.
--
--   The script names the databases: a context lands on a database with its own name, unless an inclusion
--   statement gives it another name with @NAMED@. The compose file takes every name from the environment,
--   with the name in the script as the default, so that a deployment can run a version on a database with
--   another name. The application of a database gets the name of its own database, and the names of the
--   databases it reads, each under the name that the script gives it.
module Ampersand.Commands.Deploy
  ( deploy,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree (contextDatabases, foreignContextFiles, foreignContexts, foreignLabelViews, ownDatabase)
import Ampersand.FSpec
import Ampersand.Misc.HasClasses
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (isAlphaNum, toLower, toUpper)
import RIO.Directory
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

-- | One database of the system, as the deployment sees it, with its application.
data Member = Member
  { -- | the name of the service, and of the directory with its Dockerfile
    mId :: !Text,
    -- | the name of the context that installs the database and runs its application
    mName :: !Text,
    -- | the names of the contexts on the database, the one that installs it first
    mContexts :: ![Text],
    -- | the value for the option --context of the compiler; empty for the context in the root file
    mOption :: !Text,
    -- | the variable of the environment that holds the name of the database
    mVar :: !Text,
    -- | the name of the database, unless the environment says otherwise
    mDefaultDb :: !Text,
    -- | the databases it reads: the name by which the generated SQL refers to each of them, and its service
    mReaches :: ![(Text, Text)],
    -- | the port on which its application answers
    mPort :: !Int
  }

deploy ::
  (HasOutputFile env, HasDirOutput env, HasFSpecGenOpts env, HasLogFunc env) =>
  FSpec ->
  RIO env ()
deploy fSpec = do
  env <- ask
  roots <- mapM (liftIO . canonicalizePath) . NE.toList . getRoots $ view rootFileL env
  outDir <- liftIO . canonicalizePath $ view dirOutputL env
  let composeFile = outDir </> view outputfileL env
      rootFile = case roots of
        h : _ -> h
        [] -> fatal "A script has a root file."
      -- Everything that an image needs has to be inside the directory that Docker builds from.
      scripts = commonDirectory (map takeDirectory (rootFile : map (T.unpack . snd) (foreignContextFiles (metas fSpec))))
      buildDir = commonDirectory [scripts, outDir]
      members = membersOf fSpec
      write file content = do
        liftIO $ createDirectoryIfMissing True (takeDirectory file)
        writeFileUtf8 file content
        logInfo $ "  " <> display (T.pack (makeRelative outDir file))
  logInfo $ "Generating the deployment of " <> display (T.intercalate ", " (concatMap mContexts members)) <> " in " <> display (T.pack outDir)
  when (buildDir /= scripts)
    . logWarn
    $ "Docker builds from "
    <> display (T.pack buildDir)
    <> ", the directory that contains both your scripts and the output. "
    <> "Choose an output directory inside "
    <> display (T.pack scripts)
    <> " to keep what Docker reads small."
  forM_ members $ \m ->
    write (outDir </> T.unpack (mId m) </> "Dockerfile")
      $ dockerfile m (toPosix (makeRelative buildDir scripts)) (toPosix (makeRelative scripts rootFile))
  write composeFile (compose members (toPosix (relativeTo outDir buildDir)) (toPosix (makeRelative buildDir outDir)))
  write (outDir </> ".env.example") (envExample members)
  write (outDir </> "db-init" </> "01-grant.sql") grants
  write (outDir </> "install.sh") (installScript members)
  -- The generated README tells the reader to run ./install.sh, so the file has to be executable.
  liftIO $ do
    perms <- getPermissions (outDir </> "install.sh")
    setPermissions (outDir </> "install.sh") (setOwnerExecutable True perms)
  write (outDir </> "README.md") (readme members (T.pack (takeFileName composeFile)))
  liftIO $ createDirectoryIfMissing True outDir
  BL.writeFile (outDir </> "system.json") (JSON.encodePretty (systemJson members) <> "\n")
  logInfo "  system.json"
  where
    toPosix = T.map (\c -> if c == '\\' then '/' else c) . T.pack

-- | The databases of the system: the database of the compiled context first, then the databases it reaches.
--   Every database has one application, that of the context which includes the other contexts on it;
--   the compiler has seen to it that such a context exists, and that no two databases read each other.
membersOf :: FSpec -> [Member]
membersOf fSpec = zipWith withPort [0 ..] (viewer : map other otherDatabases)
  where
    ms = metas fSpec
    others = foreignContexts ms
    views = foreignLabelViews ms
    databases = contextDatabases ms
    ownDb = fromMaybe (fullName fSpec) (ownDatabase ms)
    databaseOf :: Text -> Text
    databaseOf lbl = case [db | (l, db, _) <- databases, l == lbl] of
      db : _ -> db
      [] -> fatal ("The context " <> lbl <> " lands on no database.")
    nameOf :: Text -> Text
    nameOf lbl = case [nm | (l, nm) <- others, l == lbl] of
      nm : _ -> nm
      [] -> fatal ("The context " <> lbl <> " is not part of the system.")
    -- The other databases, each with the label of the context that installs it: the context whose tables carry no prefix there.
    otherDatabases :: [(Text, Text)]
    otherDatabases = L.nub [(db, l) | (l, db, "") <- databases, l /= "", db /= ownDb]
    -- The contexts on a database besides the one that installs it.
    alsoOn :: Text -> Text -> [Text]
    alsoOn db root = [nameOf l | (l, db', _) <- databases, l /= "", l /= root, db' == db]
    viewer =
      Member
        { mId = serviceName ownDb,
          mName = fullName fSpec,
          mContexts = fullName fSpec : alsoOn ownDb "",
          mOption = "",
          mVar = variable ownDb,
          mDefaultDb = ownDb,
          mReaches = L.nub [(db, serviceName db) | (l, db, _) <- databases, l /= "", db /= ownDb],
          mPort = 0
        }
    other (db, root) =
      Member
        { mId = serviceName db,
          mName = nameOf root,
          mContexts = nameOf root : alsoOn db root,
          mOption = root,
          mVar = variable db,
          mDefaultDb = db,
          mReaches = L.nub [(databaseOf here, serviceName (databaseOf here)) | (owner, here, _) <- views, owner == root, databaseOf here /= db],
          mPort = 0
        }
    withPort :: Int -> Member -> Member
    withPort i m = m {mPort = 8080 + i}
    serviceName = T.map toLower . T.filter isAlphaNum
    variable = (<> "_DBNAME") . T.map toUpper . T.filter isAlphaNum

-- | The order in which the applications are installed: a database after every database it reads.
installOrder :: [Member] -> [Member]
installOrder = go []
  where
    go done [] = reverse done
    go done todo = case L.partition (all ((`elem` map mId done) . snd) . mReaches) todo of
      ([], rest) -> reverse done <> rest
      (ready, rest) -> go (reverse ready <> done) rest

dockerfile :: Member -> Text -> Text -> Text
dockerfile m scripts rootFile =
  T.unlines
    $ [ "# The application of the database " <> mDefaultDb m <> ", with the context" <> pluralS (mContexts m) <> " " <> T.intercalate ", " (mContexts m) <> ".",
        "# Generated by `ampersand deploy`. Build it with the compose file in the directory above,",
        "# which sets the directory that Docker builds from.",
        "ARG FRAMEWORK_IMAGE=ampersandtarski/prototype-framework:latest",
        "FROM ${FRAMEWORK_IMAGE}",
        "",
        "# The scripts of the whole system: a context is compiled together with the contexts it reaches.",
        "COPY " <> (if scripts == "." then "." else scripts <> "/") <> " /usr/local/project/",
        "",
        "# The backend of this database. Every concept gets a table, because other databases may read it."
      ]
    <> proto ["--no-frontend"] "/var/www/backend"
    <> [ "",
         "# The frontend of this database."
       ]
    <> proto ["--frontend-version Angular", "--no-backend"] "/var/www/frontend/src/app/generated"
    <> [ "",
         "WORKDIR /var/www/frontend",
         "RUN npx ng build",
         "RUN cp -r /var/www/frontend/dist/prototype-frontend/* /var/www/html"
       ]
  where
    pluralS xs = if length xs == 1 then "" else "s"
    proto flags dir =
      [ "RUN ampersand proto " <> T.unwords flags <> " /usr/local/project/" <> rootFile <> " \\"
      ]
        <> ["  --context " <> mOption m <> " \\" | not (T.null (mOption m))]
        <> [ "  --all-concept-tables \\",
             "  --proto-dir " <> dir <> " \\",
             "  --crud-defaults cRud \\",
             "  --verbose"
           ]

compose :: [Member] -> Text -> Text -> Text
compose members buildDir outDir =
  T.unlines
    $ [ "# A system of contexts: " <> T.intercalate ", " (concatMap mContexts members) <> ".",
        "# Generated by `ampersand deploy`.",
        "#",
        "# Every database has an application of its own, on one database server.",
        "# The name of a database comes from the environment, with the name in the script as the default; see .env.example.",
        "#",
        "#   docker compose up -d --build",
        "#   ./install.sh",
        "volumes:",
        "  db-data:",
        "",
        "services:"
      ]
    <> concatMap service members
    <> [ "  db:",
         "    image: mariadb:10.6",
         "    restart: unless-stopped",
         "    command: [\"--lower-case-table-names=1\", \"--sql-mode=ANSI,TRADITIONAL\"]",
         "    environment:",
         "      - MYSQL_ROOT_PASSWORD=${MYSQL_ROOT_PASSWORD:-ampersand}",
         "      - MYSQL_USER=ampersand",
         "      - MYSQL_PASSWORD=${MYSQL_AMPERSAND_PASSWORD:-ampersand}",
         "    volumes:",
         "      - db-data:/var/lib/mysql",
         "      - ./db-init:/docker-entrypoint-initdb.d",
         "    healthcheck:",
         "      test: [\"CMD\", \"mysqladmin\", \"ping\", \"-h\", \"127.0.0.1\", \"-uampersand\", \"-p${MYSQL_AMPERSAND_PASSWORD:-ampersand}\", \"--silent\"]",
         "      interval: 2s",
         "      timeout: 5s",
         "      retries: 45"
       ]
  where
    dbName m = "${" <> mVar m <> ":-" <> mDefaultDb m <> "}"
    byId i = case filter ((== i) . mId) members of
      m : _ -> m
      [] -> fatal ("The context " <> i <> " is not part of the system.")
    service m =
      [ "  " <> mId m <> ":",
        "    platform: linux/amd64",
        "    build:",
        "      context: " <> buildDir,
        "      dockerfile: " <> (if outDir == "." then "" else outDir <> "/") <> mId m <> "/Dockerfile",
        "      args:",
        "        - FRAMEWORK_IMAGE=${FRAMEWORK_IMAGE:-ampersandtarski/prototype-framework:latest}",
        "    restart: unless-stopped",
        "    depends_on:",
        "      db:",
        "        condition: service_healthy"
      ]
        <> concat [["      " <> i <> ":", "        condition: service_started"] | (_, i) <- mReaches m]
        <> [ "    ports:",
             "      - \"${" <> T.dropEnd 7 (mVar m) <> "_PORT:-" <> tshow (mPort m) <> "}:80\"",
             "    environment:",
             "      - AMPERSAND_DEBUG_MODE=${AMPERSAND_DEBUG_MODE:-true}",
             "      - AMPERSAND_DBHOST=db",
             "      - AMPERSAND_DBUSER=ampersand",
             "      - AMPERSAND_DBPASS=${MYSQL_AMPERSAND_PASSWORD:-ampersand}",
             "      - AMPERSAND_DBNAME=" <> dbName m,
             "      - AMPERSAND_SERVER_URL=http://localhost:${" <> T.dropEnd 7 (mVar m) <> "_PORT:-" <> tshow (mPort m) <> "}"
           ]
        <> [ "      - AMPERSAND_CONTEXT_DBNAMES=" <> T.intercalate ";" [lbl <> "=" <> dbName (byId i) | (lbl, i) <- mReaches m]
             | not (null (mReaches m))
           ]
        <> [""]

envExample :: [Member] -> Text
envExample members =
  T.unlines
    $ [ "# The names of the databases. The default is the name in the script:",
        "# the name of the context, or the name that the inclusion statement gives with NAMED.",
        "# A new name means a new, empty database: deploy the new version next to the old one and migrate."
      ]
    <> [mVar m <> "=" <> mDefaultDb m | m <- members]
    <> [ "",
         "# The ports on which the applications answer."
       ]
    <> [T.dropEnd 7 (mVar m) <> "_PORT=" <> tshow (mPort m) | m <- members]
    <> [ "",
         "MYSQL_ROOT_PASSWORD=ampersand",
         "MYSQL_AMPERSAND_PASSWORD=ampersand",
         "# The prototype framework. It has to know the setting AMPERSAND_CONTEXT_DBNAMES,",
         "# and its compiler has to know systems of contexts (the option --context).",
         "FRAMEWORK_IMAGE=ampersandtarski/prototype-framework:latest"
       ]

grants :: Text
grants =
  T.unlines
    [ "-- Every application creates its own database when it is installed,",
      "-- and reads the databases of the contexts it reaches.",
      "GRANT ALL PRIVILEGES ON *.* TO 'ampersand'@'%';"
    ]

installScript :: [Member] -> Text
installScript members =
  T.unlines
    $ [ "#!/usr/bin/env bash",
        "# Install the applications of the system, one per database.",
        "# A database is installed after every database it reads, because it has views on their tables.",
        "# Installing an application replaces its database, and no other.",
        "set -euo pipefail",
        "cd \"$(dirname \"$0\")\"",
        "[ -f .env ] && set -a && . ./.env && set +a",
        "",
        "install() {",
        "  echo \"Installing $1 on port $2 ...\"",
        "  for _ in $(seq 1 60); do",
        "    curl -s -o /dev/null \"http://localhost:$2/\" && break",
        "    sleep 2",
        "  done",
        "  curl -sS -f \"http://localhost:$2/api/v1/admin/installer${INSTALLER_OPTIONS:-}\" >/dev/null",
        "}",
        ""
      ]
    <> ["install " <> mId m <> " \"${" <> T.dropEnd 7 (mVar m) <> "_PORT:-" <> tshow (mPort m) <> "}\"" | m <- installOrder members]

readme :: [Member] -> Text -> Text
readme members composeFile =
  T.unlines
    $ [ "# Deployment of a system of contexts",
        "",
        "This directory was generated by `ampersand deploy`.",
        "It runs " <> tshow (length members) <> " database" <> (if length members == 1 then "" else "s") <> ", each with an application of its own,",
        "next to one MariaDB server.",
        "",
        "| Database (default) | Contexts | Service | Port | Reads the databases of |",
        "| --- | --- | --- | --- | --- |"
      ]
    <> [ "| " <> T.intercalate " | " [mDefaultDb m, T.intercalate ", " (mContexts m), mId m, tshow (mPort m), T.intercalate ", " (map snd (mReaches m))] <> " |"
         | m <- members
       ]
    <> [ "",
         "## Running it",
         "",
         "```bash",
         "docker compose -f " <> composeFile <> " up -d --build",
         "./install.sh",
         "```",
         "",
         "`install.sh` installs the applications in this order: " <> T.intercalate ", " (map mId (installOrder members)) <> ".",
         "A database is installed after every database it reads, because it contains views on their tables.",
         "",
         "The images build on the prototype framework that `FRAMEWORK_IMAGE` names.",
         "That framework has to know the setting `AMPERSAND_CONTEXT_DBNAMES`,",
         "and the compiler in it has to know systems of contexts.",
         "With an older framework, the installation of an application that reads another database fails on an unknown database.",
         "",
         "## The names of the databases",
         "",
         "The script names the databases: a context lands on a database with its own name,",
         "unless an inclusion statement gives it another name with `NAMED`, and contexts with the same name share one database.",
         "The compose file takes every name from the environment, with the name in the script as the default,",
         "and gives every application the name of its own database and the names of the databases it reads.",
         "Copy `.env.example` to `.env` to set them.",
         "A new name means a new database, so a new version runs next to the old one until its data has been migrated.",
         "",
         "## Files",
         "",
         "- `" <> composeFile <> "`: the services.",
         "- `<service>/Dockerfile`: the image of one application.",
         "- `db-init/01-grant.sql`: the rights of the database user.",
         "- `install.sh`: installs every application.",
         "- `system.json`: the same facts for a program to read."
       ]

systemJson :: [Member] -> JSON.Value
systemJson members =
  JSON.object
    [ "contexts"
        JSON..= [ JSON.object
                    [ "service" JSON..= mId m,
                      "context" JSON..= mName m,
                      "contexts" JSON..= mContexts m,
                      "label" JSON..= mOption m,
                      "databaseVariable" JSON..= mVar m,
                      "defaultDatabase" JSON..= mDefaultDb m,
                      "port" JSON..= mPort m,
                      "reaches" JSON..= [JSON.object ["database" JSON..= db, "service" JSON..= i] | (db, i) <- mReaches m]
                    ]
                  | m <- members
                ],
      "installOrder" JSON..= map mId (installOrder members)
    ]

-- | The deepest directory that contains all given directories.
commonDirectory :: [FilePath] -> FilePath
commonDirectory dirs = case map splitDirectories dirs of
  [] -> "."
  h : tl -> joinPath (foldl' common h tl)
  where
    common xs ys = map fst (takeWhile (uncurry (==)) (zip xs ys))

-- | The path of a directory, relative to another directory that need not contain it.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo start target
  | null path = "."
  | otherwise = joinPath path
  where
    shared = length (splitDirectories (commonDirectory [start, target]))
    path = replicate (length (splitDirectories start) - shared) ".." <> drop shared (splitDirectories target)
