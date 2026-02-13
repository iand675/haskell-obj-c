{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , abortParsingSelector
  , allowedExternalEntityURLsSelector
  , columnNumberSelector
  , delegateSelector
  , externalEntityResolvingPolicySelector
  , initWithContentsOfURLSelector
  , initWithDataSelector
  , initWithStreamSelector
  , lineNumberSelector
  , parseSelector
  , parserErrorSelector
  , publicIDSelector
  , setAllowedExternalEntityURLsSelector
  , setDelegateSelector
  , setExternalEntityResolvingPolicySelector
  , setShouldProcessNamespacesSelector
  , setShouldReportNamespacePrefixesSelector
  , setShouldResolveExternalEntitiesSelector
  , shouldProcessNamespacesSelector
  , shouldReportNamespacePrefixesSelector
  , shouldResolveExternalEntitiesSelector
  , systemIDSelector

  -- * Enum types
  , NSXMLParserExternalEntityResolvingPolicy(NSXMLParserExternalEntityResolvingPolicy)
  , pattern NSXMLParserResolveExternalEntitiesNever
  , pattern NSXMLParserResolveExternalEntitiesNoNetwork
  , pattern NSXMLParserResolveExternalEntitiesSameOriginOnly
  , pattern NSXMLParserResolveExternalEntitiesAlways

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSXMLParser nsxmlParser, IsNSURL url) => nsxmlParser -> url -> IO (Id NSXMLParser)
initWithContentsOfURL nsxmlParser url =
  sendOwnedMessage nsxmlParser initWithContentsOfURLSelector (toNSURL url)

-- | @- initWithData:@
initWithData :: (IsNSXMLParser nsxmlParser, IsNSData data_) => nsxmlParser -> data_ -> IO (Id NSXMLParser)
initWithData nsxmlParser data_ =
  sendOwnedMessage nsxmlParser initWithDataSelector (toNSData data_)

-- | @- initWithStream:@
initWithStream :: (IsNSXMLParser nsxmlParser, IsNSInputStream stream) => nsxmlParser -> stream -> IO (Id NSXMLParser)
initWithStream nsxmlParser stream =
  sendOwnedMessage nsxmlParser initWithStreamSelector (toNSInputStream stream)

-- | @- parse@
parse :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
parse nsxmlParser =
  sendMessage nsxmlParser parseSelector

-- | @- abortParsing@
abortParsing :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO ()
abortParsing nsxmlParser =
  sendMessage nsxmlParser abortParsingSelector

-- | @- delegate@
delegate :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO RawId
delegate nsxmlParser =
  sendMessage nsxmlParser delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSXMLParser nsxmlParser => nsxmlParser -> RawId -> IO ()
setDelegate nsxmlParser value =
  sendMessage nsxmlParser setDelegateSelector value

-- | @- shouldProcessNamespaces@
shouldProcessNamespaces :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldProcessNamespaces nsxmlParser =
  sendMessage nsxmlParser shouldProcessNamespacesSelector

-- | @- setShouldProcessNamespaces:@
setShouldProcessNamespaces :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldProcessNamespaces nsxmlParser value =
  sendMessage nsxmlParser setShouldProcessNamespacesSelector value

-- | @- shouldReportNamespacePrefixes@
shouldReportNamespacePrefixes :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldReportNamespacePrefixes nsxmlParser =
  sendMessage nsxmlParser shouldReportNamespacePrefixesSelector

-- | @- setShouldReportNamespacePrefixes:@
setShouldReportNamespacePrefixes :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldReportNamespacePrefixes nsxmlParser value =
  sendMessage nsxmlParser setShouldReportNamespacePrefixesSelector value

-- | @- externalEntityResolvingPolicy@
externalEntityResolvingPolicy :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO NSXMLParserExternalEntityResolvingPolicy
externalEntityResolvingPolicy nsxmlParser =
  sendMessage nsxmlParser externalEntityResolvingPolicySelector

-- | @- setExternalEntityResolvingPolicy:@
setExternalEntityResolvingPolicy :: IsNSXMLParser nsxmlParser => nsxmlParser -> NSXMLParserExternalEntityResolvingPolicy -> IO ()
setExternalEntityResolvingPolicy nsxmlParser value =
  sendMessage nsxmlParser setExternalEntityResolvingPolicySelector value

-- | @- allowedExternalEntityURLs@
allowedExternalEntityURLs :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSSet)
allowedExternalEntityURLs nsxmlParser =
  sendMessage nsxmlParser allowedExternalEntityURLsSelector

-- | @- setAllowedExternalEntityURLs:@
setAllowedExternalEntityURLs :: (IsNSXMLParser nsxmlParser, IsNSSet value) => nsxmlParser -> value -> IO ()
setAllowedExternalEntityURLs nsxmlParser value =
  sendMessage nsxmlParser setAllowedExternalEntityURLsSelector (toNSSet value)

-- | @- parserError@
parserError :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSError)
parserError nsxmlParser =
  sendMessage nsxmlParser parserErrorSelector

-- | @- shouldResolveExternalEntities@
shouldResolveExternalEntities :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO Bool
shouldResolveExternalEntities nsxmlParser =
  sendMessage nsxmlParser shouldResolveExternalEntitiesSelector

-- | @- setShouldResolveExternalEntities:@
setShouldResolveExternalEntities :: IsNSXMLParser nsxmlParser => nsxmlParser -> Bool -> IO ()
setShouldResolveExternalEntities nsxmlParser value =
  sendMessage nsxmlParser setShouldResolveExternalEntitiesSelector value

-- | @- publicID@
publicID :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSString)
publicID nsxmlParser =
  sendMessage nsxmlParser publicIDSelector

-- | @- systemID@
systemID :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO (Id NSString)
systemID nsxmlParser =
  sendMessage nsxmlParser systemIDSelector

-- | @- lineNumber@
lineNumber :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO CLong
lineNumber nsxmlParser =
  sendMessage nsxmlParser lineNumberSelector

-- | @- columnNumber@
columnNumber :: IsNSXMLParser nsxmlParser => nsxmlParser -> IO CLong
columnNumber nsxmlParser =
  sendMessage nsxmlParser columnNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSXMLParser)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSXMLParser)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @initWithStream:@
initWithStreamSelector :: Selector '[Id NSInputStream] (Id NSXMLParser)
initWithStreamSelector = mkSelector "initWithStream:"

-- | @Selector@ for @parse@
parseSelector :: Selector '[] Bool
parseSelector = mkSelector "parse"

-- | @Selector@ for @abortParsing@
abortParsingSelector :: Selector '[] ()
abortParsingSelector = mkSelector "abortParsing"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @shouldProcessNamespaces@
shouldProcessNamespacesSelector :: Selector '[] Bool
shouldProcessNamespacesSelector = mkSelector "shouldProcessNamespaces"

-- | @Selector@ for @setShouldProcessNamespaces:@
setShouldProcessNamespacesSelector :: Selector '[Bool] ()
setShouldProcessNamespacesSelector = mkSelector "setShouldProcessNamespaces:"

-- | @Selector@ for @shouldReportNamespacePrefixes@
shouldReportNamespacePrefixesSelector :: Selector '[] Bool
shouldReportNamespacePrefixesSelector = mkSelector "shouldReportNamespacePrefixes"

-- | @Selector@ for @setShouldReportNamespacePrefixes:@
setShouldReportNamespacePrefixesSelector :: Selector '[Bool] ()
setShouldReportNamespacePrefixesSelector = mkSelector "setShouldReportNamespacePrefixes:"

-- | @Selector@ for @externalEntityResolvingPolicy@
externalEntityResolvingPolicySelector :: Selector '[] NSXMLParserExternalEntityResolvingPolicy
externalEntityResolvingPolicySelector = mkSelector "externalEntityResolvingPolicy"

-- | @Selector@ for @setExternalEntityResolvingPolicy:@
setExternalEntityResolvingPolicySelector :: Selector '[NSXMLParserExternalEntityResolvingPolicy] ()
setExternalEntityResolvingPolicySelector = mkSelector "setExternalEntityResolvingPolicy:"

-- | @Selector@ for @allowedExternalEntityURLs@
allowedExternalEntityURLsSelector :: Selector '[] (Id NSSet)
allowedExternalEntityURLsSelector = mkSelector "allowedExternalEntityURLs"

-- | @Selector@ for @setAllowedExternalEntityURLs:@
setAllowedExternalEntityURLsSelector :: Selector '[Id NSSet] ()
setAllowedExternalEntityURLsSelector = mkSelector "setAllowedExternalEntityURLs:"

-- | @Selector@ for @parserError@
parserErrorSelector :: Selector '[] (Id NSError)
parserErrorSelector = mkSelector "parserError"

-- | @Selector@ for @shouldResolveExternalEntities@
shouldResolveExternalEntitiesSelector :: Selector '[] Bool
shouldResolveExternalEntitiesSelector = mkSelector "shouldResolveExternalEntities"

-- | @Selector@ for @setShouldResolveExternalEntities:@
setShouldResolveExternalEntitiesSelector :: Selector '[Bool] ()
setShouldResolveExternalEntitiesSelector = mkSelector "setShouldResolveExternalEntities:"

-- | @Selector@ for @publicID@
publicIDSelector :: Selector '[] (Id NSString)
publicIDSelector = mkSelector "publicID"

-- | @Selector@ for @systemID@
systemIDSelector :: Selector '[] (Id NSString)
systemIDSelector = mkSelector "systemID"

-- | @Selector@ for @lineNumber@
lineNumberSelector :: Selector '[] CLong
lineNumberSelector = mkSelector "lineNumber"

-- | @Selector@ for @columnNumber@
columnNumberSelector :: Selector '[] CLong
columnNumberSelector = mkSelector "columnNumber"

