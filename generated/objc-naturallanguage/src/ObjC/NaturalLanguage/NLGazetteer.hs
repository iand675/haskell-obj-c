{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLGazetteer@.
module ObjC.NaturalLanguage.NLGazetteer
  ( NLGazetteer
  , IsNLGazetteer(..)
  , gazetteerWithContentsOfURL_error
  , initWithContentsOfURL_error
  , initWithData_error
  , initWithDictionary_language_error
  , labelForString
  , writeGazetteerForDictionary_language_toURL_error
  , language
  , data_
  , gazetteerWithContentsOfURL_errorSelector
  , initWithContentsOfURL_errorSelector
  , initWithData_errorSelector
  , initWithDictionary_language_errorSelector
  , labelForStringSelector
  , writeGazetteerForDictionary_language_toURL_errorSelector
  , languageSelector
  , dataSelector


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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ gazetteerWithContentsOfURL:error:@
gazetteerWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLGazetteer)
gazetteerWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLGazetteer"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "gazetteerWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNLGazetteer nlGazetteer, IsNSURL url, IsNSError error_) => nlGazetteer -> url -> error_ -> IO (Id NLGazetteer)
initWithContentsOfURL_error nlGazetteer  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nlGazetteer (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:error:@
initWithData_error :: (IsNLGazetteer nlGazetteer, IsNSData data_, IsNSError error_) => nlGazetteer -> data_ -> error_ -> IO (Id NLGazetteer)
initWithData_error nlGazetteer  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nlGazetteer (mkSelector "initWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDictionary:language:error:@
initWithDictionary_language_error :: (IsNLGazetteer nlGazetteer, IsNSDictionary dictionary, IsNSString language, IsNSError error_) => nlGazetteer -> dictionary -> language -> error_ -> IO (Id NLGazetteer)
initWithDictionary_language_error nlGazetteer  dictionary language error_ =
withObjCPtr dictionary $ \raw_dictionary ->
  withObjCPtr language $ \raw_language ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nlGazetteer (mkSelector "initWithDictionary:language:error:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- labelForString:@
labelForString :: (IsNLGazetteer nlGazetteer, IsNSString string) => nlGazetteer -> string -> IO (Id NSString)
labelForString nlGazetteer  string =
withObjCPtr string $ \raw_string ->
    sendMsg nlGazetteer (mkSelector "labelForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ writeGazetteerForDictionary:language:toURL:error:@
writeGazetteerForDictionary_language_toURL_error :: (IsNSDictionary dictionary, IsNSString language, IsNSURL url, IsNSError error_) => dictionary -> language -> url -> error_ -> IO Bool
writeGazetteerForDictionary_language_toURL_error dictionary language url error_ =
  do
    cls' <- getRequiredClass "NLGazetteer"
    withObjCPtr dictionary $ \raw_dictionary ->
      withObjCPtr language $ \raw_language ->
        withObjCPtr url $ \raw_url ->
          withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "writeGazetteerForDictionary:language:toURL:error:") retCULong [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- language@
language :: IsNLGazetteer nlGazetteer => nlGazetteer -> IO (Id NSString)
language nlGazetteer  =
  sendMsg nlGazetteer (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsNLGazetteer nlGazetteer => nlGazetteer -> IO (Id NSData)
data_ nlGazetteer  =
  sendMsg nlGazetteer (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @gazetteerWithContentsOfURL:error:@
gazetteerWithContentsOfURL_errorSelector :: Selector
gazetteerWithContentsOfURL_errorSelector = mkSelector "gazetteerWithContentsOfURL:error:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithDictionary:language:error:@
initWithDictionary_language_errorSelector :: Selector
initWithDictionary_language_errorSelector = mkSelector "initWithDictionary:language:error:"

-- | @Selector@ for @labelForString:@
labelForStringSelector :: Selector
labelForStringSelector = mkSelector "labelForString:"

-- | @Selector@ for @writeGazetteerForDictionary:language:toURL:error:@
writeGazetteerForDictionary_language_toURL_errorSelector :: Selector
writeGazetteerForDictionary_language_toURL_errorSelector = mkSelector "writeGazetteerForDictionary:language:toURL:error:"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

