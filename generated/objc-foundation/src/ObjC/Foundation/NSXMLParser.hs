{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSXMLParser@.
module ObjC.Foundation.NSXMLParser
  ( NSXMLParser
  , IsNSXMLParser(..)
  , initWithContentsOfURL
  , initWithData
  , initWithStream
  , parse
  , abortParsing
  , shouldProcessNamespaces
  , setShouldProcessNamespaces
  , shouldReportNamespacePrefixes
  , setShouldReportNamespacePrefixes
  , externalEntityResolvingPolicy
  , setExternalEntityResolvingPolicy
  , allowedExternalEntityURLs
  , setAllowedExternalEntityURLs
  , parserError
  , shouldResolveExternalEntities
  , setShouldResolveExternalEntities
  , publicID
  , systemID
  , lineNumber
  , columnNumber
  , initWithContentsOfURLSelector
  , initWithDataSelector
  , initWithStreamSelector
  , parseSelector
  , abortParsingSelector
  , shouldProcessNamespacesSelector
  , setShouldProcessNamespacesSelector
  , shouldReportNamespacePrefixesSelector
  , setShouldReportNamespacePrefixesSelector
  , externalEntityResolvingPolicySelector
  , setExternalEntityResolvingPolicySelector
  , allowedExternalEntityURLsSelector
  , setAllowedExternalEntityURLsSelector
  , parserErrorSelector
  , shouldResolveExternalEntitiesSelector
  , setShouldResolveExternalEntitiesSelector
  , publicIDSelector
  , systemIDSelector
  , lineNumberSelector
  , columnNumberSelector

  -- * Enum types
  , NSXMLParserExternalEntityResolvingPolicy(NSXMLParserExternalEntityResolvingPolicy)
  , pattern NSXMLParserResolveExternalEntitiesNever
  , pattern NSXMLParserResolveExternalEntitiesNoNetwork
  , pattern NSXMLParserResolveExternalEntitiesSameOriginOnly
  , pattern NSXMLParserResolveExternalEntitiesAlways

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSXMLParser nsxmlParser, IsNSURL url) => nsxmlParser -> url -> IO (Id NSXMLParser)
initWithContentsOfURL nsxmlParser  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsxmlParser (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSXMLParser nsxmlParser, IsNSData data_) => nsxmlParser -> data_ -> IO (Id NSXMLParser)
initWithData nsxmlParser  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg nsxmlParser (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithStream:@
initWithStream :: (IsNSXMLParser nsxmlParser, IsNSInputStream stream) => nsxmlParser -> stream -> IO (Id NSXMLParser)
initWithStream nsxmlParser  stream =
withObjCPtr stream $ \raw_stream ->
    sendMsg nsxmlParser (mkSelector "initWithStream:") (retPtr retVoid) [argPtr (castPtr raw_stream :: Ptr ())] >>= ownedObject . castPtr

-- | @- parse@
parse :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
parse nsxmlParser  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlParser (mkSelector "parse") retCULong []

-- | @- abortParsing@
abortParsing :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO ()
abortParsing nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "abortParsing") retVoid []

-- | @- shouldProcessNamespaces@
shouldProcessNamespaces :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldProcessNamespaces nsxmlParser  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlParser (mkSelector "shouldProcessNamespaces") retCULong []

-- | @- setShouldProcessNamespaces:@
setShouldProcessNamespaces :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldProcessNamespaces nsxmlParser  value =
  sendMsg nsxmlParser (mkSelector "setShouldProcessNamespaces:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldReportNamespacePrefixes@
shouldReportNamespacePrefixes :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldReportNamespacePrefixes nsxmlParser  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlParser (mkSelector "shouldReportNamespacePrefixes") retCULong []

-- | @- setShouldReportNamespacePrefixes:@
setShouldReportNamespacePrefixes :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldReportNamespacePrefixes nsxmlParser  value =
  sendMsg nsxmlParser (mkSelector "setShouldReportNamespacePrefixes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- externalEntityResolvingPolicy@
externalEntityResolvingPolicy :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO NSXMLParserExternalEntityResolvingPolicy
externalEntityResolvingPolicy nsxmlParser  =
  fmap (coerce :: CULong -> NSXMLParserExternalEntityResolvingPolicy) $ sendMsg nsxmlParser (mkSelector "externalEntityResolvingPolicy") retCULong []

-- | @- setExternalEntityResolvingPolicy:@
setExternalEntityResolvingPolicy :: IsNSXMLParser nsxmlParser => nsxmlParser -> NSXMLParserExternalEntityResolvingPolicy -> IO ()
setExternalEntityResolvingPolicy nsxmlParser  value =
  sendMsg nsxmlParser (mkSelector "setExternalEntityResolvingPolicy:") retVoid [argCULong (coerce value)]

-- | @- allowedExternalEntityURLs@
allowedExternalEntityURLs :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSSet)
allowedExternalEntityURLs nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "allowedExternalEntityURLs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAllowedExternalEntityURLs:@
setAllowedExternalEntityURLs :: (IsNSXMLParser nsxmlParser, IsNSSet value) => nsxmlParser -> value -> IO ()
setAllowedExternalEntityURLs nsxmlParser  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlParser (mkSelector "setAllowedExternalEntityURLs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parserError@
parserError :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSError)
parserError nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "parserError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldResolveExternalEntities@
shouldResolveExternalEntities :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldResolveExternalEntities nsxmlParser  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlParser (mkSelector "shouldResolveExternalEntities") retCULong []

-- | @- setShouldResolveExternalEntities:@
setShouldResolveExternalEntities :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldResolveExternalEntities nsxmlParser  value =
  sendMsg nsxmlParser (mkSelector "setShouldResolveExternalEntities:") retVoid [argCULong (if value then 1 else 0)]

-- | @- publicID@
publicID :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSString)
publicID nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "publicID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- systemID@
systemID :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSString)
systemID nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "systemID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lineNumber@
lineNumber :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO CLong
lineNumber nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "lineNumber") retCLong []

-- | @- columnNumber@
columnNumber :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO CLong
columnNumber nsxmlParser  =
  sendMsg nsxmlParser (mkSelector "columnNumber") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithStream:@
initWithStreamSelector :: Selector
initWithStreamSelector = mkSelector "initWithStream:"

-- | @Selector@ for @parse@
parseSelector :: Selector
parseSelector = mkSelector "parse"

-- | @Selector@ for @abortParsing@
abortParsingSelector :: Selector
abortParsingSelector = mkSelector "abortParsing"

-- | @Selector@ for @shouldProcessNamespaces@
shouldProcessNamespacesSelector :: Selector
shouldProcessNamespacesSelector = mkSelector "shouldProcessNamespaces"

-- | @Selector@ for @setShouldProcessNamespaces:@
setShouldProcessNamespacesSelector :: Selector
setShouldProcessNamespacesSelector = mkSelector "setShouldProcessNamespaces:"

-- | @Selector@ for @shouldReportNamespacePrefixes@
shouldReportNamespacePrefixesSelector :: Selector
shouldReportNamespacePrefixesSelector = mkSelector "shouldReportNamespacePrefixes"

-- | @Selector@ for @setShouldReportNamespacePrefixes:@
setShouldReportNamespacePrefixesSelector :: Selector
setShouldReportNamespacePrefixesSelector = mkSelector "setShouldReportNamespacePrefixes:"

-- | @Selector@ for @externalEntityResolvingPolicy@
externalEntityResolvingPolicySelector :: Selector
externalEntityResolvingPolicySelector = mkSelector "externalEntityResolvingPolicy"

-- | @Selector@ for @setExternalEntityResolvingPolicy:@
setExternalEntityResolvingPolicySelector :: Selector
setExternalEntityResolvingPolicySelector = mkSelector "setExternalEntityResolvingPolicy:"

-- | @Selector@ for @allowedExternalEntityURLs@
allowedExternalEntityURLsSelector :: Selector
allowedExternalEntityURLsSelector = mkSelector "allowedExternalEntityURLs"

-- | @Selector@ for @setAllowedExternalEntityURLs:@
setAllowedExternalEntityURLsSelector :: Selector
setAllowedExternalEntityURLsSelector = mkSelector "setAllowedExternalEntityURLs:"

-- | @Selector@ for @parserError@
parserErrorSelector :: Selector
parserErrorSelector = mkSelector "parserError"

-- | @Selector@ for @shouldResolveExternalEntities@
shouldResolveExternalEntitiesSelector :: Selector
shouldResolveExternalEntitiesSelector = mkSelector "shouldResolveExternalEntities"

-- | @Selector@ for @setShouldResolveExternalEntities:@
setShouldResolveExternalEntitiesSelector :: Selector
setShouldResolveExternalEntitiesSelector = mkSelector "setShouldResolveExternalEntities:"

-- | @Selector@ for @publicID@
publicIDSelector :: Selector
publicIDSelector = mkSelector "publicID"

-- | @Selector@ for @systemID@
systemIDSelector :: Selector
systemIDSelector = mkSelector "systemID"

-- | @Selector@ for @lineNumber@
lineNumberSelector :: Selector
lineNumberSelector = mkSelector "lineNumber"

-- | @Selector@ for @columnNumber@
columnNumberSelector :: Selector
columnNumberSelector = mkSelector "columnNumber"

