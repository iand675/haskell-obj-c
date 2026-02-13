{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , gazetteerWithContentsOfURL_errorSelector
  , initWithContentsOfURL_errorSelector
  , initWithData_errorSelector
  , initWithDictionary_language_errorSelector
  , labelForStringSelector
  , languageSelector
  , writeGazetteerForDictionary_language_toURL_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ gazetteerWithContentsOfURL:error:@
gazetteerWithContentsOfURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NLGazetteer)
gazetteerWithContentsOfURL_error url error_ =
  do
    cls' <- getRequiredClass "NLGazetteer"
    sendClassMessage cls' gazetteerWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsNLGazetteer nlGazetteer, IsNSURL url, IsNSError error_) => nlGazetteer -> url -> error_ -> IO (Id NLGazetteer)
initWithContentsOfURL_error nlGazetteer url error_ =
  sendOwnedMessage nlGazetteer initWithContentsOfURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- initWithData:error:@
initWithData_error :: (IsNLGazetteer nlGazetteer, IsNSData data_, IsNSError error_) => nlGazetteer -> data_ -> error_ -> IO (Id NLGazetteer)
initWithData_error nlGazetteer data_ error_ =
  sendOwnedMessage nlGazetteer initWithData_errorSelector (toNSData data_) (toNSError error_)

-- | @- initWithDictionary:language:error:@
initWithDictionary_language_error :: (IsNLGazetteer nlGazetteer, IsNSDictionary dictionary, IsNSString language, IsNSError error_) => nlGazetteer -> dictionary -> language -> error_ -> IO (Id NLGazetteer)
initWithDictionary_language_error nlGazetteer dictionary language error_ =
  sendOwnedMessage nlGazetteer initWithDictionary_language_errorSelector (toNSDictionary dictionary) (toNSString language) (toNSError error_)

-- | @- labelForString:@
labelForString :: (IsNLGazetteer nlGazetteer, IsNSString string) => nlGazetteer -> string -> IO (Id NSString)
labelForString nlGazetteer string =
  sendMessage nlGazetteer labelForStringSelector (toNSString string)

-- | @+ writeGazetteerForDictionary:language:toURL:error:@
writeGazetteerForDictionary_language_toURL_error :: (IsNSDictionary dictionary, IsNSString language, IsNSURL url, IsNSError error_) => dictionary -> language -> url -> error_ -> IO Bool
writeGazetteerForDictionary_language_toURL_error dictionary language url error_ =
  do
    cls' <- getRequiredClass "NLGazetteer"
    sendClassMessage cls' writeGazetteerForDictionary_language_toURL_errorSelector (toNSDictionary dictionary) (toNSString language) (toNSURL url) (toNSError error_)

-- | @- language@
language :: IsNLGazetteer nlGazetteer => nlGazetteer -> IO (Id NSString)
language nlGazetteer =
  sendMessage nlGazetteer languageSelector

-- | @- data@
data_ :: IsNLGazetteer nlGazetteer => nlGazetteer -> IO (Id NSData)
data_ nlGazetteer =
  sendMessage nlGazetteer dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @gazetteerWithContentsOfURL:error:@
gazetteerWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NLGazetteer)
gazetteerWithContentsOfURL_errorSelector = mkSelector "gazetteerWithContentsOfURL:error:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NLGazetteer)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id NLGazetteer)
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @initWithDictionary:language:error:@
initWithDictionary_language_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSError] (Id NLGazetteer)
initWithDictionary_language_errorSelector = mkSelector "initWithDictionary:language:error:"

-- | @Selector@ for @labelForString:@
labelForStringSelector :: Selector '[Id NSString] (Id NSString)
labelForStringSelector = mkSelector "labelForString:"

-- | @Selector@ for @writeGazetteerForDictionary:language:toURL:error:@
writeGazetteerForDictionary_language_toURL_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSURL, Id NSError] Bool
writeGazetteerForDictionary_language_toURL_errorSelector = mkSelector "writeGazetteerForDictionary:language:toURL:error:"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

