{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generate what is needed to run a system of contexts.
--
--   Every context of a system has a database of its own and a backend of its own.
--   The command @ampersand deploy@ writes, for the context in the root file and every context it reaches:
--   a Dockerfile that builds the application of that context on the prototype framework,
--   and one compose file that runs the applications next to one database server.
--
--   The compiler does not know the names of the databases. They are facts of the deployment:
--   the compose file takes them from the environment, with a default that consists of
--   the name of the context and a version. The application of a context gets the name of its own database,
--   and the names of the databases of the contexts it reaches, each under the label by which it knows that context.
module Ampersand.Commands.Deploy
  ( deploy,
  )
where

import Ampersand.Basics
import Ampersand.Core.ParseTree (foreignContextFiles, foreignContexts, foreignLabelViews)
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

-- | One context of the system, as the deployment sees it.
data Member = Member
  { -- | the name of the service, and of the directory with its Dockerfile
    mId :: !Text,
    -- | the name of the context
    mName :: !Text,
    -- | the value for the option --context of the compiler; empty for the context in the root file
    mOption :: !Text,
    -- | the variable of the environment that holds the name of its database
    mVar :: !Text,
    -- | the name of its database, unless the environment says otherwise
    mDefaultDb :: !Text,
    -- | the contexts it reaches: the label by which it knows each of them, and that context
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
  logInfo $ "Generating the deployment of " <> display (T.intercalate ", " (map mName members)) <> " in " <> display (T.pack outDir)
  forM_ members $ \m ->
    write (outDir </> T.unpack (mId m) </> "Dockerfile")
      $ dockerfile m (toPosix (makeRelative buildDir scripts)) (toPosix (makeRelative scripts rootFile))
  write composeFile (compose members (toPosix (relativeTo outDir buildDir)) (toPosix (makeRelative buildDir outDir)))
  write (outDir </> ".env.example") (envExample members)
  write (outDir </> "db-init" </> "01-grant.sql") grants
  write (outDir </> "install.sh") (installScript members)
  write (outDir </> "README.md") (readme members (T.pack (takeFileName composeFile)))
  liftIO $ createDirectoryIfMissing True outDir
  BL.writeFile (outDir </> "system.json") (JSON.encodePretty (systemJson members) <> "\n")
  logInfo "  system.json"
  where
    toPosix = T.map (\c -> if c == '\\' then '/' else c) . T.pack

-- | The contexts of the system: the compiled context first, then the contexts it reaches.
--   A context comes after every context it reaches, so that the list is an order of installation when reversed.
membersOf :: FSpec -> [Member]
membersOf fSpec = zipWith withPort [0 ..] (viewer : map other others)
  where
    others = foreignContexts (metas fSpec)
    views = foreignLabelViews (metas fSpec)
    names = fullName fSpec : map snd others
    viewer =
      Member
        { mId = serviceName (fullName fSpec),
          mName = fullName fSpec,
          mOption = "",
          mVar = variable (fullName fSpec),
          mDefaultDb = defaultDb 0 (fullName fSpec),
          mReaches = [(lbl, serviceName lbl) | (lbl, _) <- others],
          mPort = 0
        }
    other (lbl, nm) =
      Member
        { mId = serviceName lbl,
          mName = nm,
          mOption = lbl,
          mVar = variable lbl,
          mDefaultDb = defaultDb (1 + length (takeWhile ((/= lbl) . fst) others)) nm,
          mReaches = [(there, serviceName here) | (owner, here, there) <- views, owner == lbl],
          mPort = 0
        }
    withPort :: Int -> Member -> Member
    withPort i m = m {mPort = 8080 + i}
    -- The name of a database contains the name of the context and a version.
    -- Two versions of one context have the same name. Without anything else to go by, they are numbered from 1.
    defaultDb :: Int -> Text -> Text
    defaultDb i nm = T.map toLower (T.filter isAlphaNum nm) <> "_" <> tshow (1 + length (filter (== nm) (take i names)))
    serviceName = T.map toLower . T.filter isAlphaNum
    variable = (<> "_DBNAME") . T.map toUpper . T.filter isAlphaNum

-- | The order in which the applications are installed: a context after every context it reaches.
installOrder :: [Member] -> [Member]
installOrder = L.sortOn (length . mReaches)

dockerfile :: Member -> Text -> Text -> Text
dockerfile m scripts rootFile =
  T.unlines
    $ [ "# The application of the context " <> mName m <> ".",
        "# Generated by `ampersand deploy`. Build it with the compose file in the directory above,",
        "# which sets the directory that Docker builds from.",
        "ARG FRAMEWORK_IMAGE=ampersandtarski/prototype-framework:latest",
        "FROM ${FRAMEWORK_IMAGE}",
        "",
        "# The scripts of the whole system: a context is compiled together with the contexts it reaches.",
        "COPY " <> (if scripts == "." then "." else scripts <> "/") <> " /usr/local/project/",
        "",
        "# The backend of this context. Every concept gets a table, because other contexts may read it."
      ]
    <> proto ["--no-frontend"] "/var/www/backend"
    <> [ "",
         "# The frontend of this context."
       ]
    <> proto ["--frontend-version Angular", "--no-backend"] "/var/www/frontend/src/app/generated"
    <> [ "",
         "WORKDIR /var/www/frontend",
         "RUN npx ng build",
         "RUN cp -r /var/www/frontend/dist/prototype-frontend/* /var/www/html"
       ]
  where
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
    $ [ "# A system of contexts: " <> T.intercalate ", " (map mName members) <> ".",
        "# Generated by `ampersand deploy`.",
        "#",
        "# Every context has an application of its own and a database of its own, on one database server.",
        "# The name of a database comes from the environment; see .env.example.",
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
    $ [ "# The names of the databases, one per context.",
        "# A name contains the name of the context and its version, so that someone who looks",
        "# in the database knows which version it holds. Take the version from your own release process.",
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
        "# Install the applications of the system, each on its own database.",
        "# A context is installed after every context it reaches, because it reads their tables.",
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
        "It runs " <> tshow (length members) <> " contexts, each with an application of its own and a database of its own,",
        "next to one MariaDB server.",
        "",
        "| Context | Service | Database (default) | Port | Reads the databases of |",
        "| --- | --- | --- | --- | --- |"
      ]
    <> [ "| " <> T.intercalate " | " [mName m, mId m, mDefaultDb m, tshow (mPort m), T.intercalate ", " (map snd (mReaches m))] <> " |"
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
         "A context is installed after every context it reaches, because its database contains views on their tables.",
         "",
         "## The names of the databases",
         "",
         "The compiler does not know the name of a database. The compose file takes it from the environment,",
         "and gives every application the name of its own database and the names of the databases it reads.",
         "Copy `.env.example` to `.env` to set them.",
         "A name contains the name of the context and a version.",
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
                      "label" JSON..= mOption m,
                      "databaseVariable" JSON..= mVar m,
                      "defaultDatabase" JSON..= mDefaultDb m,
                      "port" JSON..= mPort m,
                      "reaches" JSON..= [JSON.object ["label" JSON..= lbl, "service" JSON..= i] | (lbl, i) <- mReaches m]
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
