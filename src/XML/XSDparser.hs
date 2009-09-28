
module XSDparser --(pXSDocument, xsdkeywordstxt, xsdkeywordsops, xsdspecialchars, xsdopchars)
  where
   import UU_Parsing
   import UU_Scanner
   import Collection  (Collection(..))
   import Auxiliaries (sort)
   import Data.XML
--   import Text.XML.HXT.Arrow.Pickle.Schema   -- Onderdeel van HXT  (cabal install hxt) --Nog niet gebruikt, is (nog?) niet portable genoeg. 
--parseXSD :: String     -- ^ The string to be parsed
--         -> Options     -- ^ flags to be taken into account
--         -> String      -- ^ The name of the .xsd file (used for error messages)
--         -> IO(XSDocument) -- ^ The IO monad with the context. 
--parseXSD xsdstring flags fnFull =
--    do { slRes <- parseIO pXSDocument (scan xsdkeywordstxt xsdkeywordsops xsdspecialchars xsdopchars fnFull initPos xsdstring)
--	   ; return slRes
--       }
            
   
      
--De volgende EBRN notatie komt van http://www.w3.org/TR/xmlschema-ref/ 
--[1]   	SchemaComponentDesignator	   ::=   	AbsoluteSchemaComponentDesignator |  RelativeSchemaComponentDesignator
--[2]   	AbsoluteSchemaComponentDesignator	   ::=   	SchemaDesignator '#' RelativeSchemaComponentDesignator
--[3]   	SchemaDesignator	   ::=   	AnyURI
--[4]   	AnyURI	   ::=   	Char*	/* IRI reference: no fragment identifier */
--[5]   	RelativeSchemaComponentDesignator	   ::=   	XmlnsPointerPart* XscdPointerPart
--[6]   	XmlnsPointerPart	   ::=   	'xmlns' '(' XmlnsSchemeData ')'
--[7]   	XscdPointerPart	   ::=   	'xscd' '(' SchemaComponentPath ')' 
--[8]   	SchemaComponentPath	   ::=   	( SchemaStep | StepSepator RelativeSchemaComponentPath ) ('/' ExtensionAccessor)?
--[9]   	SchemaStep	   ::=   	'/'
--[10]   	RelativeSchemaComponentPath	   ::=   	Step | Step StepSeparator RelativeSchemaComponentPath
--[11]   	StepSeparator	   ::=   	'/' | ComponentAxisSeparator
--[12]   	ComponentAxisSeparator	   ::=   	'//'
--[13]   	Step	   ::=   	Axis NameTest Predicate? | AbbrevStep
--[14]   	AbbrevStep	   ::=   	AbbrevAttributeStep | AbbrevElementStep | AbbrevTypeStep | AbbrevCurrentComponentStep |
--[15]   	AbbrevAttributeStep	   ::=   	'@' NameTest Predicate?
--[16]   	AbbrevElementStep	   ::=   	NameTest Predicate?
--[17]   	AbbrevTypeStep	   ::=   	'~' NameTest Predicate?
--[18]   	AbbrevCurrentComponentStep	   ::=   	'.' Predicate?
--[19]   	Axis	   ::=   	'schemaAttribute' '::' | 'schemaElement' '::' | 'type' '::' | 'attributeGroup' '::' | 'group' '::' | 'identityConstraint' '::' | 'assertion' '::' | 'alternative' '::' | 'notation' '::' | 'model' '::' | 'anyAttribute' '::' | 'any' '::' | 'facet' '::' | 'scope' '::' | 'context' '::' | 'substitutionGroup' '::' | 'baseType' '::' | 'itemType' '::' | 'memberType' '::' | 'primitiveType' '::' | 'key' '::' | 'annotation' '::' | 'component' '::' | 'currentComponent' '::' | 'attributeUse' '::' | 'particle' '::' | ExtensionAxis
--[20]   	NameTest	   ::=   	QName | WildcardNameTest | '0'
--[21]   	WildcardNameTest	   ::=   	'*'
--[22]   	ExtensionAccessor	   ::=   	QName '(' ')'
--[23]   	ExtensionAxis	   ::=   	QName '::'
--[24]   	Predicate	   ::=   	'[' [0-9]+ ']'
--[25]   	CurrentComponentAxis	   ::=   	'currentComponent' '::
--[26]   	AnnotationAxis	   ::=   	'annotation' '::'
--[27]   	SchemaAttributeAxis	   ::=   	'schemaAttribute' '::'
--[28]   	SchemaElementAxis	   ::=   	'schemaElement' '::'
--[29]   	TypeAxis	   ::=   	'type' '::'
--[30]   	AttributeGroupAxis	   ::=   	'attributeGroup' '::'
--[31]   	GroupAxis	   ::=   	'group' '::'
--[32]   	IdentityConstraintAxis	   ::=   	'identityConstraint' '::'
--[33]   	AssertionAxis	   ::=   	'assertion' '::'
--[34]   	AlternativeAxis	   ::=   	'alternative' '::'
--[35]   	NotationAxis	   ::=   	'notation' '::'
--[36]   	ModelAxis	   ::=   	'model' '::'
--[37]   	AnyAttributeAxis	   ::=   	'anyAttribute' '::'
--[38]   	AnyAxis	   ::=   	'any' '::'
--[39]   	FacetAxis	   ::=   	'facet' '::'
--[40]   	ScopeAxis	   ::=   	'scope' '::'
--[41]   	ContextAxis	   ::=   	'context' '::'
--[42]   	SubstitutionGroupAxis	   ::=   	'substitutionGroup' '::'
--[43]   	BaseTypeAxis	   ::=   	'baseType' '::'
--[44]   	ItemTypeAxis	   ::=   	'itemType' '::'
--[45]   	MemberTypeAxis	   ::=   	'memberType' '::'
--[46]   	PrimitiveTypeAxis	   ::=   	'primitiveType' '::'
--[47]   	KeyAxis	   ::=   	'key' '::'
--[48]   	AttributeUseAxis	   ::=   	'attributeUse' '::
--[49]   	ParticleAxis	   ::=   	'particle' '::
--[50]   	ComponentAxis	   ::=   	'component' '::'

-- Nu volgt een voor een een parser voor het desbetreffende token: 

--[1]   	SchemaComponentDesignator	   ::=   	AbsoluteSchemaComponentDesignator |  RelativeSchemaComponentDesignator
   pSchemaComponentDesignator :: Parser Token XAtt
   pSchemaComponentDesignator =  pAbsoluteSchemaComponentDesignator <|>
                                 pRelativeSchemaComponentDesignator

--[2]   	AbsoluteSchemaComponentDesignator	   ::=   	SchemaDesignator '#' RelativeSchemaComponentDesignator
   pAbsoluteSchemaComponentDesignator :: Parser Token XAtt
   pAbsoluteSchemaComponentDesignator = ret <$> pSchemaDesignator <$ pKey "#"  <*> pRelativeSchemaComponentDesignator
                                        where ret :: String -> String -> XAtt
                                              ret a b = Att { attName  = "xmlns"  -- TODO Nakijken of dit juist is
                                                            , attValue = a ++ "#" ++ b
                                                            }
                                                            
--[3]   	SchemaDesignator	   ::=   	AnyURI
   pSchemaDesignator :: Parser Token String
   pSchemaDesignator = pAnyURI

--[4]   	AnyURI	   ::=   	Char*	/* IRI reference: no fragment identifier */
   pAnyURI :: Parser Token String
   pAnyURI = pString


--[5]   	RelativeSchemaComponentDesignator	   ::=   	XmlnsPointerPart* XscdPointerPart
   pRelativeSchemaComponentDesignator :: Parser Token String
   pRelativeSchemaComponentDesignator = pList pXmlnsPointerPart <*> pXscdPointerPart

--[6]   	XmlnsPointerPart	   ::=   	'xmlns' '(' XmlnsSchemeData ')'
   pXmlnsPointerPart :: Parser Token String
   pXmlnsPointerPart = f <$ pKey "xmlns" <*> pBracks pXmlnsSchemeData
                        where 
                          f :: String -> String
                          f a = "xmlns " ++ "(" ++ a ++ ")"

--[7]   	XscdPointerPart	   ::=   	'xscd' '(' SchemaComponentPath ')' 
   pXscdPointerPart :: Parser Token String
   pXscdPointerPart = f <$ pKey "xscd" <*> pBracks pSchemaComponentPath
                        where 
                          f :: String -> String
                          f a = "xscd " ++ "(" ++ a ++ ")"

--[8]   	SchemaComponentPath	   ::=   	( SchemaStep | StepSepator RelativeSchemaComponentPath ) ('/' ExtensionAccessor)?
   pSchemaComponentPath :: Parser Token String
   pSchemaComponentPath = ( pSchemaStep <|>
                            (pSchemaStep <*> pRelativeSchemaComponentPath )
                          )  

--[9]   	SchemaStep	   ::=   	'/'
   pSchemaStep :: Parser Token String
   pSchemaStep = pKey "/"

--[10]   	RelativeSchemaComponentPath	   ::=   	Step | Step StepSeparator RelativeSchemaComponentPath
   pRelativeSchemaComponentPath :: Parser Token String
   pRelativeSchemaComponentPath = pStep  <|>
                                  (f pStep <*>
                                     pStepSeparator <*>
                                     pRelativeSchemaComponentPath
                                     )
                                    where f :: String -> String -> String -> String
                                          f s ss rscp = "TODO - pRelativeSchemaComponentPath"   

--[11]   	StepSeparator	   ::=   	'/' | ComponentAxisSeparator
   pStepSeparator :: Parser Token String
   pStepSeparator = pKey "/" <|>
                    pComponentAxisSeparator

--[12]   	ComponentAxisSeparator	   ::=   	'//'
   pComponentAxisSeparator :: Parser Token String
   pComponentAxisSeparator = pKey "//"

--[13]   	Step	   ::=   	Axis NameTest Predicate? | AbbrevStep
   pStep :: Parser Token String
   pStep = (f <$> pAxis <*> pNameTest <*> (pPredicate `opt` ""))
           <|> pAbbrevStep
             where f a b c = "TODO - pStep"

--[14]   	AbbrevStep	   ::=   	AbbrevAttributeStep | AbbrevElementStep | AbbrevTypeStep | AbbrevCurrentComponentStep |
   pAbbrevStep :: Parser Token String
   pAbbrevStep = pAbbrevAttributeStep <|>
                 pAbbrevElementStep   <|>
                 pAbbrevTypeStep      <|>
                 pAbbrevCurrentComponentStep

--[15]   	AbbrevAttributeStep	   ::=   	'@' NameTest Predicate?
   pAbbrevAttributeStep :: Parser Token String
   pAbbrevAttributeStep = f <$> pKey "@" <*> pNameTest <*> (pPredicate `opt` "")
                           where f:: String -> String -> String -> String
                                 f a b c = "TODO - pAbbrevAttributeStep"

--[16]   	AbbrevElementStep	   ::=   	NameTest Predicate?
   pAbbrevElementStep :: Parser Token String
   pAbbrevElementStep = f <$> pNameTest <*> (pPredicate `opt` "")
                        where f a b = "TODO - pAbbrevElementStep"

--[17]   	AbbrevTypeStep	   ::=   	'~' NameTest Predicate?
   pAbbrevTypeStep :: Parser Token String
   pAbbrevTypeStep = f <$> pKey "~" <*> pNameTest <*> (pPredicate `opt` "")
                           where f:: String -> String -> String -> String
                                 f a b c = "TODO - pAbbrevTypeStep"

--[18]   	AbbrevCurrentComponentStep	   ::=   	'.' Predicate?
   pAbbrevCurrentComponentStep :: Parser Token String
   pAbbrevCurrentComponentStep = f <$> pKey "." <*> (pPredicate `opt` "")
                           where f:: String -> String -> String
                                 f a b c = "TODO - pAbbrevCurrentComponentStep"
                                 
--[19]   	Axis	   ::=   	'schemaAttribute' '::' 
--                            | 'schemaElement'   '::'
--                            | 'type'            '::'
--                            | 'attributeGroup'  '::'
--                            | 'group'           '::'
--                            | 'identityConstraint' '::'
--                            | 'assertion' '::'
--                            | 'alternative' '::' 
--                            | 'notation' '::' 
--                            | 'model' '::' 
--                            | 'anyAttribute' '::' 
--                            | 'any' '::' 
--                            | 'facet' '::' 
--                            | 'scope' '::' 
--                            | 'context' '::' 
--                            | 'substitutionGroup' '::' 
--                            | 'baseType' '::' 
--                            | 'itemType' '::' 
--                            | 'memberType' '::' 
--                            | 'primitiveType' '::' 
--                            | 'key' '::' 
--                            | 'annotation' '::' 
--                            | 'component' '::' 
--                            | 'currentComponent' '::' 
--                            | 'attributeUse' '::' 
--                            | 'particle' '::' 
--                            | ExtensionAxis
   pAxis :: Parser Token String
   pAxis = f <$> pKey "schemaAttribute" *> pKey "::" 
            where f a b = "TODO - pAxis"
   

--[20]   	NameTest	   ::=   	QName | WildcardNameTest | '0'
   pNameTest :: Parser Token String
   pNameTest = pQname <|>
               pWildcardNameTest <|>
               pKey "0"
                
--[21]   	WildcardNameTest	   ::=   	'*'
   pWildcardNameTest :: Parser Token String
   pWildcardNameTest = pKey "*"

--[22]   	ExtensionAccessor	   ::=   	QName '(' ')'



--[23]   	ExtensionAxis	   ::=   	QName '::'



--[24]   	Predicate	   ::=   	'[' [0-9]+ ']'
   pPredicate :: Parser Token String
   pPredicate = pBracks pInteger


--[25]   	CurrentComponentAxis	   ::=   	'currentComponent' '::



--[26]   	AnnotationAxis	   ::=   	'annotation' '::'



--[27]   	SchemaAttributeAxis	   ::=   	'schemaAttribute' '::'



--[28]   	SchemaElementAxis	   ::=   	'schemaElement' '::'



--[29]   	TypeAxis	   ::=   	'type' '::'



--[30]   	AttributeGroupAxis	   ::=   	'attributeGroup' '::'



--[31]   	GroupAxis	   ::=   	'group' '::'



--[32]   	IdentityConstraintAxis	   ::=   	'identityConstraint' '::'



--[33]   	AssertionAxis	   ::=   	'assertion' '::'



--[34]   	AlternativeAxis	   ::=   	'alternative' '::'



--[35]   	NotationAxis	   ::=   	'notation' '::'



--[36]   	ModelAxis	   ::=   	'model' '::'



--[37]   	AnyAttributeAxis	   ::=   	'anyAttribute' '::'



--[38]   	AnyAxis	   ::=   	'any' '::'



--[39]   	FacetAxis	   ::=   	'facet' '::'



--[40]   	ScopeAxis	   ::=   	'scope' '::'



--[41]   	ContextAxis	   ::=   	'context' '::'



--[42]   	SubstitutionGroupAxis	   ::=   	'substitutionGroup' '::'



--[43]   	BaseTypeAxis	   ::=   	'baseType' '::'



--[44]   	ItemTypeAxis	   ::=   	'itemType' '::'



--[45]   	MemberTypeAxis	   ::=   	'memberType' '::'



--[46]   	PrimitiveTypeAxis	   ::=   	'primitiveType' '::'



--[47]   	KeyAxis	   ::=   	'key' '::'



--[48]   	AttributeUseAxis	   ::=   	'attributeUse' '::



--[49]   	ParticleAxis	   ::=   	'particle' '::



--[50]   	ComponentAxis	   ::=   	'component' '::'




   xsdkeywordstxt :: [String]
   xsdkeywordstxt       = [ ]
   
   xsdkeywordsops :: [String]
   xsdkeywordsops       = [ ] -- "-|", "|-", ":-", "-:", "-", "->", ">", "=", "~", "+", ";", "!", "*", "::", ":", "\\/", "/\\" ]
   xsdspecialchars :: String
   xsdspecialchars      = "()[].,{}"
   xsdopchars :: String
   xsdopchars           = rd (sort (concat xsdkeywordsops))





   pXSDocument    :: Parser Token XSDocument
   pXSDocument  = pString -- "Not implemented yet"

   type XSDocument = String