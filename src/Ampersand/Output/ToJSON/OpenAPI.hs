{-# LANGUAGE OverloadedStrings #-}

-- | Generates an OpenAPI 3.0 description of the REST API that the prototype
--   framework serves for this context. The structure follows the agreed
--   reference (see the OpenAPI design note):
--
--     * one path + GET per interface; PrototypeContext.* interfaces are excluded
--       (they are injected by the framework, not part of the user's script);
--     * field visibility follows interface CRUD: only readable (R) fields appear
--       in the read schema; create/update/delete (C/U/D) drive PATCH/POST/DELETE
--       and are annotated per field as @x-crud@;
--     * univalence is used ONLY to distinguish a list from a single value;
--       required/nullable are deliberately NOT derived from relation totality,
--       since the API contract is an interface-level notion;
--     * write request/response shapes follow the PrototypeFramework
--       ResourceController / Resource implementation (JSON-Patch + WriteResult).
--
--   Like the other generators in this directory it consumes the FSpec; unlike
--   them it builds the aeson 'Value' directly, because OpenAPI keys such as
--   @$ref@, @200@ and @application/json@ cannot be expressed as record fields.
module Ampersand.Output.ToJSON.OpenAPI (openAPIToJSON) where

import Ampersand.ADL1
import Ampersand.FSpec.ToFSpec.NormalForms (conjNF)
import Ampersand.Output.ToJSON.JSONutils
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair)
import Data.Char (isAlphaNum)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.Text as T

openAPIToJSON :: env -> FSpec -> BL.ByteString
openAPIToJSON env fSpec = encodePretty'' (openApiDoc env fSpec)

encodePretty'' :: (ToJSON a) => a -> BL.ByteString
encodePretty'' =
  encodePretty'
    Config
      { confIndent = Spaces 4,
        confCompare = compare,
        confNumFormat = Generic,
        confTrailingNewline = False
      }

-- * Top-level document

openApiDoc :: env -> FSpec -> Value
openApiDoc env fSpec =
  object
    [ "openapi" .= ("3.0.3" :: Text),
      "info"
        .= object
          [ "title" .= (ctxName <> " — Ampersand prototype API"),
            "version" .= ("0.4.0" :: Text),
            "description" .= descr
          ],
      "servers"
        .= ( [ object
                 [ "url" .= ("http://localhost/api/v1" :: Text),
                   "description" .= ("Local prototype server" :: Text)
                 ]
             ] ::
               [Value]
           ),
      "tags"
        .= ( [ object
                 [ "name" .= ctxName,
                   "description" .= ("Interfaces defined in the context" :: Text)
                 ]
             ] ::
               [Value]
           ),
      "paths" .= object (ifcPathPairs <> createPathPairs),
      "components" .= object ["schemas" .= object (sharedSchemas <> ifcSchemaPairs)]
    ]
  where
    ctxName = fullName fSpec
    descr =
      ( "Reference OpenAPI description of the REST API served by the Ampersand prototype "
          <> "framework. Field visibility follows interface CRUD (R = read; C/U/D = write). "
          <> "Univalence only distinguishes a list from a single value; totality is not used."
      ) ::
        Text

    apps :: [Interface]
    apps = filter (not . isProtoCtx) (interfaceS fSpec <> interfaceG fSpec)
    isProtoCtx ifc = "PrototypeContext." `T.isPrefixOf` fullName ifc

    infos = map mkInfo apps
    ifcPathPairs = [Key.fromText (iiPath i) .= object (iiMethods i) | i <- infos]
    ifcSchemaPairs = [Key.fromText (iiSchemaName i) .= iiSchema i | i <- infos]
    createConcepts = L.nub [c | Just c <- map iiCreate infos]
    createPathPairs =
      [Key.fromText ("/resource/" <> c) .= object ["post" .= createOp c] | c <- createConcepts]

    createOp c =
      object
        [ "tags" .= ([ctxName] :: [Text]),
          "operationId" .= ("create" <> alnum c),
          "summary" .= ("Create a new " <> c <> " atom (returns its identifier)."),
          "description"
            .= ( "Creates a new atom of this concept and returns its id; not committed until populated." ::
                   Text
               ),
          "responses" .= responses200 "The new atom id." "NewResourceId"
        ]

    -- ---- per interface --------------------------------------------------
    mkInfo :: Interface -> IfcInfo
    mkInfo ifc = IfcInfo path methods createC schemaName schema
      where
        nm = fullName ifc
        topOd = substituteReferenceObjectDef fSpec (ifcObj ifc)
        rootC = case getExpressionRelation (conjNF env (objExpression topOd)) of
          Just (s, _, _, _) -> s
          Nothing -> source (conjNF env (objExpression topOd))
        rootName = text1ToText (idWithoutType' rootC)
        hasParam = rootName /= "SESSION" && rootName /= "ONE"
        path
          | rootName == "SESSION" = "/resource/SESSION/1/" <> nm
          | rootName == "ONE" = "/resource/ONE/1/" <> nm
          | otherwise = "/resource/" <> rootName <> "/{resourceId}/" <> nm
        (hasC, hasU, hasD) = treeRights topOd
        schemaName = "Ifc_" <> schemaIdent nm
        roles = map (text1ToText . idWithoutType') (ifcRoles ifc)
        pathParams = [pathParam rootName | hasParam]
        schema = readSchema topOd
        createC = if hasC && hasParam then Just rootName else Nothing

        getO =
          object
            [ "tags" .= ([ctxName] :: [Text]),
              "operationId" .= ("get" <> alnum nm),
              "summary" .= ("Read interface \"" <> label ifc <> "\" (root concept: " <> rootName <> ")."),
              "description"
                .= ( "Ampersand INTERFACE "
                       <> nm
                       <> ". Only readable (R) fields appear; write rights are PATCH/DELETE/POST and per-field x-crud."
                   ),
              "parameters" .= (pathParams <> queryParams),
              "responses" .= responses200 "The resource." schemaName,
              "x-ampersand-isAPI" .= ifcIsAPI ifc,
              "x-ampersand-rootConcept" .= rootName,
              "x-ampersand-roles" .= roles
            ]
        patchO =
          object
            $ [ "tags" .= ([ctxName] :: [Text]),
                "operationId" .= ("patch" <> alnum nm),
                "summary" .= ("Update \"" <> label ifc <> "\" via JSON-Patch operations."),
                "description"
                  .= ( "Array of patch operations; the allowed op per field follows x-crud (U -> replace/add/remove, C -> create)." ::
                         Text
                     )
              ]
            <> ["parameters" .= pathParams | hasParam]
            <> [ "requestBody"
                   .= object
                     [ "required" .= True,
                       "content" .= object ["application/json" .= object ["schema" .= ref "Patch"]]
                     ],
                 "responses" .= responses200 "Patch result." "WriteResult"
               ]
        deleteO =
          object
            [ "tags" .= ([ctxName] :: [Text]),
              "operationId" .= ("delete" <> alnum nm),
              "summary" .= ("Delete the addressed " <> rootName <> " atom (and all pairs it occurs in)."),
              "parameters" .= [pathParam rootName],
              "responses" .= responses200 "Delete result." "DeleteResult"
            ]
        methods =
          ("get" .= getO)
            : ["patch" .= patchO | hasU || hasC]
              <> ["delete" .= deleteO | hasD && hasParam]

    -- ---- read schema ----------------------------------------------------
    readSchema :: ObjectDef -> Value
    readSchema topOd =
      if isUni (conjNF env (objExpression topOd))
        then body
        else object ["type" .= ("array" :: Text), "items" .= body]
      where
        body = object (boxPairs topOd)

    boxPairs :: ObjectDef -> [Pair]
    boxPairs od =
      [ "type" .= ("object" :: Text),
        "properties" .= object (metaProps <> childProps od),
        "required" .= (["_id_"] :: [Text]),
        "additionalProperties" .= True
      ]

    childProps :: ObjectDef -> [Pair]
    childProps od =
      [Key.fromText (fieldName c) .= fieldSchema c | c <- kids od, crudR (objcrud c)]

    fieldName :: ObjectDef -> Text
    fieldName c = maybe "" (text1ToText . escapeIdentifier) (objPlainName c)

    -- direct, reference-substituted child objects of a box
    kids :: ObjectDef -> [ObjectDef]
    kids od = case objmsub od of
      Just b@Box {} -> [substituteReferenceObjectDef fSpec o | BxExpr o <- siObjs b]
      _ -> []

    treeRights :: ObjectDef -> (Bool, Bool, Bool)
    treeRights od = foldr orT (crudC c, crudU c, crudD c) (map treeRights (kids od))
      where
        c = objcrud od
        orT (a, b, d) (x, y, z) = (a || x, b || y, d || z)

    fieldSchema :: ObjectDef -> Value
    fieldSchema c =
      if isUni nExpr
        then object (uniBase <> ["nullable" .= True] <> ext)
        else object (["type" .= ("array" :: Text), "items" .= itemsV] <> ext)
      where
        nExpr = conjNF env (objExpression c)
        (tgtC, mDecl) = case getExpressionRelation nExpr of
          Just (_, decl, t, _) -> (t, Just decl)
          Nothing -> (target nExpr, Nothing)
        crd = objcrud c
        ext =
          ("x-crud" .= crudObj crd)
            : ["x-ampersand-relation" .= tshow d | Just d <- [mDecl]]
        -- A field is rendered as a boolean only when it is backed by a declared
        -- PROP relation (matching the @prop@ flag in relations.json, which is
        -- @isProp (EDcD decl)@). Deriving prop-ness from the normalized field
        -- expression would over-flag: complex expressions can normalize to a
        -- symmetric+antisymmetric shape without the relation being a property.
        isPropField = maybe False (isProp . EDcD) mDecl
        (itemsV, uniBase) = coreSchema c tgtC isPropField

    -- (value-for-array-items, pairs-for-single-value)
    coreSchema :: ObjectDef -> A_Concept -> Bool -> (Value, [Pair])
    coreSchema c tgtC prp = case objmsub c of
      Just Box {} -> let ps = boxPairs c in (object ps, ps)
      Just InterfaceRef {} -> (ref "AtomRef", ["allOf" .= ([ref "AtomRef"] :: [Value])])
      _
        | prp && isObjectT tgtC -> let ps = ["type" .= ("boolean" :: Text)] in (object ps, ps)
        | isObjectT tgtC -> (ref "AtomRef", ["allOf" .= ([ref "AtomRef"] :: [Value])])
        | otherwise -> let ps = scalarPairs tgtC in (object ps, ps)

    isObjectT :: A_Concept -> Bool
    isObjectT cc = case cptTType fSpec cc of
      Object -> True
      TypeOfOne -> True
      _ -> False

    scalarPairs :: A_Concept -> [Pair]
    scalarPairs cc =
      base (cptTType fSpec cc) <> ["x-ampersand-concept" .= text1ToText (idWithoutType' cc)]
      where
        base t = case t of
          Password -> ["type" .= ("string" :: Text), "format" .= ("password" :: Text)]
          Date -> ["type" .= ("string" :: Text), "format" .= ("date" :: Text)]
          DateTime -> ["type" .= ("string" :: Text), "format" .= ("date-time" :: Text)]
          Boolean -> ["type" .= ("boolean" :: Text)]
          Integer -> ["type" .= ("integer" :: Text), "format" .= ("int64" :: Text)]
          Float -> ["type" .= ("number" :: Text), "format" .= ("double" :: Text)]
          Object -> ["type" .= ("object" :: Text)]
          TypeOfOne -> ["type" .= ("object" :: Text)]
          _ -> ["type" .= ("string" :: Text)] -- Alphanumeric / Big / Huge / Binary

data IfcInfo = IfcInfo
  { iiPath :: Text,
    iiMethods :: [Pair],
    iiCreate :: Maybe Text,
    iiSchemaName :: Text,
    iiSchema :: Value
  }

-- * Pure helpers (no FSpec needed)

ref :: Text -> Value
ref n = object ["$ref" .= (("#/components/schemas/" <> n) :: Text)]

crudObj :: Cruds -> Value
crudObj crd =
  object
    [ "create" .= crudC crd,
      "read" .= crudR crd,
      "update" .= crudU crd,
      "delete" .= crudD crd
    ]

alnum :: Text -> Text
alnum = T.filter isAlphaNum

schemaIdent :: Text -> Text
schemaIdent = T.map (\c -> if isAlphaNum c || c == '_' then c else '_')

pathParam :: Text -> Value
pathParam root =
  object
    [ "name" .= ("resourceId" :: Text),
      "in" .= ("path" :: Text),
      "required" .= True,
      "schema" .= object ["type" .= ("string" :: Text)],
      "description" .= ("Atom identifier of the " <> root <> " resource.")
    ]

queryParams :: [Value]
queryParams =
  [ object
      [ "name" .= ("metaData" :: Text),
        "in" .= ("query" :: Text),
        "required" .= False,
        "schema" .= object ["type" .= ("boolean" :: Text), "default" .= True],
        "description" .= ("Include resource metadata (_path_, _ifcs_, _sortValues_)." :: Text)
      ],
    object
      [ "name" .= ("navIfc" :: Text),
        "in" .= ("query" :: Text),
        "required" .= False,
        "schema" .= object ["type" .= ("boolean" :: Text), "default" .= True],
        "description" .= ("Include navigation interfaces (_ifcs_)." :: Text)
      ]
  ]

responses200 :: Text -> Text -> Value
responses200 desc schemaName =
  object
    ( ( "200"
          .= object
            [ "description" .= desc,
              "content" .= object ["application/json" .= object ["schema" .= ref schemaName]]
            ]
      )
        : errResponses
    )

errResponses :: [Pair]
errResponses =
  [ "400" .= e "Bad request.",
    "403" .= e "Not authorized for this resource/role.",
    "404" .= e "Resource not found."
  ]
  where
    e d =
      object
        [ "description" .= (d :: Text),
          "content" .= object ["application/json" .= object ["schema" .= ref "Error"]]
        ]

-- | The resource envelope, shared by every box object and by AtomRef.
metaProps :: [Pair]
metaProps =
  [ "_id_" .= object ["type" .= ("string" :: Text), "description" .= ("Atom identifier of this resource." :: Text)],
    "_label_"
      .= object
        [ "type" .= ("string" :: Text),
          "nullable" .= True,
          "description" .= ("Human-readable label (from VIEW)." :: Text)
        ],
    "_path_" .= object ["type" .= ("string" :: Text), "description" .= ("Canonical resource path of this atom." :: Text)],
    "_view_"
      .= object
        [ "description" .= ("VIEW result: an object when a VIEW is defined, otherwise an empty array." :: Text),
          "oneOf"
            .= ( [ object ["type" .= ("object" :: Text), "additionalProperties" .= True],
                   object ["type" .= ("array" :: Text), "items" .= object []]
                 ] ::
                   [Value]
               )
        ],
    "_ifcs_"
      .= object
        [ "type" .= ("array" :: Text),
          "items" .= ref "NavIfc",
          "description" .= ("Interfaces navigable from this resource." :: Text)
        ],
    "_sortValues_"
      .= object
        [ "type" .= ("object" :: Text),
          "additionalProperties" .= True,
          "description" .= ("Per-field values used for client-side sorting." :: Text)
        ]
  ]

-- | Reusable component schemas (read envelope + write side).
sharedSchemas :: [Pair]
sharedSchemas =
  [ "NavIfc"
      .= object
        [ "type" .= ("object" :: Text),
          "properties" .= object ["id" .= strSchema, "label" .= strSchema],
          "required" .= (["id"] :: [Text])
        ],
    "AtomRef"
      .= object
        [ "type" .= ("object" :: Text),
          "description" .= ("A resource atom rendered without inlined sub-fields (full envelope)." :: Text),
          "properties" .= object metaProps,
          "required" .= (["_id_"] :: [Text]),
          "additionalProperties" .= True
        ],
    "Error"
      .= object
        [ "type" .= ("object" :: Text),
          "properties"
            .= object
              [ "error" .= intSchema,
                "msg" .= strSchema,
                "html" .= object ["type" .= ("string" :: Text), "nullable" .= True],
                "notifications" .= object ["type" .= ("object" :: Text), "additionalProperties" .= True]
              ],
          "required" .= (["error", "msg"] :: [Text])
        ],
    "Notifications"
      .= object
        [ "type" .= ("object" :: Text),
          "description" .= ("User log returned by the framework." :: Text),
          "properties"
            .= object
              [Key.fromText k .= arrOfObj | k <- ["errors", "warnings", "infos", "successes", "invariants", "signals"]]
        ],
    "PatchOp"
      .= object
        [ "type" .= ("object" :: Text),
          "description" .= ("One JSON-Patch-style operation (see Resource::patch in the framework)." :: Text),
          "properties"
            .= object
              [ "op"
                  .= object
                    [ "type" .= ("string" :: Text),
                      "enum" .= (["replace", "add", "remove", "create"] :: [Text])
                    ],
                "path" .= strSchema,
                "value" .= object []
              ],
          "required" .= (["op", "path"] :: [Text])
        ],
    "Patch"
      .= object
        [ "type" .= ("array" :: Text),
          "items" .= ref "PatchOp",
          "description" .= ("Array of patch operations (PATCH request body)." :: Text)
        ],
    "WriteResult"
      .= object
        [ "type" .= ("object" :: Text),
          "properties"
            .= object
              [ "content" .= object ["nullable" .= True],
                "patches" .= object ["type" .= ("array" :: Text), "items" .= ref "PatchOp"],
                "notifications" .= ref "Notifications",
                "invariantRulesHold" .= boolSchema,
                "isCommitted" .= boolSchema,
                "sessionRefreshAdvice" .= object ["nullable" .= True],
                "navTo" .= object ["type" .= ("string" :: Text), "nullable" .= True]
              ],
          "required" .= (["notifications", "invariantRulesHold", "isCommitted"] :: [Text])
        ],
    "DeleteResult"
      .= object
        [ "type" .= ("object" :: Text),
          "properties"
            .= object
              [ "notifications" .= ref "Notifications",
                "invariantRulesHold" .= boolSchema,
                "isCommitted" .= boolSchema,
                "sessionRefreshAdvice" .= object ["nullable" .= True],
                "navTo" .= object ["type" .= ("string" :: Text), "nullable" .= True]
              ],
          "required" .= (["notifications", "invariantRulesHold", "isCommitted"] :: [Text])
        ],
    "NewResourceId"
      .= object
        [ "type" .= ("object" :: Text),
          "properties" .= object ["_id_" .= strSchema],
          "required" .= (["_id_"] :: [Text])
        ]
  ]
  where
    strSchema = object ["type" .= ("string" :: Text)]
    intSchema = object ["type" .= ("integer" :: Text)]
    boolSchema = object ["type" .= ("boolean" :: Text)]
    arrOfObj =
      object
        [ "type" .= ("array" :: Text),
          "items" .= object ["type" .= ("object" :: Text), "additionalProperties" .= True]
        ]
