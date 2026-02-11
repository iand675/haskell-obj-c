{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSearchQueryContext@.
module ObjC.CoreSpotlight.CSSearchQueryContext
  ( CSSearchQueryContext
  , IsCSSearchQueryContext(..)
  , fetchAttributes
  , setFetchAttributes
  , filterQueries
  , setFilterQueries
  , keyboardLanguage
  , setKeyboardLanguage
  , sourceOptions
  , setSourceOptions
  , fetchAttributesSelector
  , setFetchAttributesSelector
  , filterQueriesSelector
  , setFilterQueriesSelector
  , keyboardLanguageSelector
  , setKeyboardLanguageSelector
  , sourceOptionsSelector
  , setSourceOptionsSelector

  -- * Enum types
  , CSSearchQuerySourceOptions(CSSearchQuerySourceOptions)
  , pattern CSSearchQuerySourceOptionDefault
  , pattern CSSearchQuerySourceOptionAllowMail

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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- fetchAttributes@
fetchAttributes :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSArray)
fetchAttributes csSearchQueryContext  =
  sendMsg csSearchQueryContext (mkSelector "fetchAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFetchAttributes:@
setFetchAttributes :: (IsCSSearchQueryContext csSearchQueryContext, IsNSArray value) => csSearchQueryContext -> value -> IO ()
setFetchAttributes csSearchQueryContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchQueryContext (mkSelector "setFetchAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- filterQueries@
filterQueries :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSArray)
filterQueries csSearchQueryContext  =
  sendMsg csSearchQueryContext (mkSelector "filterQueries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFilterQueries:@
setFilterQueries :: (IsCSSearchQueryContext csSearchQueryContext, IsNSArray value) => csSearchQueryContext -> value -> IO ()
setFilterQueries csSearchQueryContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchQueryContext (mkSelector "setFilterQueries:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyboardLanguage@
keyboardLanguage :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSString)
keyboardLanguage csSearchQueryContext  =
  sendMsg csSearchQueryContext (mkSelector "keyboardLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyboardLanguage:@
setKeyboardLanguage :: (IsCSSearchQueryContext csSearchQueryContext, IsNSString value) => csSearchQueryContext -> value -> IO ()
setKeyboardLanguage csSearchQueryContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg csSearchQueryContext (mkSelector "setKeyboardLanguage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceOptions@
sourceOptions :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO CSSearchQuerySourceOptions
sourceOptions csSearchQueryContext  =
  fmap (coerce :: CULong -> CSSearchQuerySourceOptions) $ sendMsg csSearchQueryContext (mkSelector "sourceOptions") retCULong []

-- | @- setSourceOptions:@
setSourceOptions :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> CSSearchQuerySourceOptions -> IO ()
setSourceOptions csSearchQueryContext  value =
  sendMsg csSearchQueryContext (mkSelector "setSourceOptions:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAttributes@
fetchAttributesSelector :: Selector
fetchAttributesSelector = mkSelector "fetchAttributes"

-- | @Selector@ for @setFetchAttributes:@
setFetchAttributesSelector :: Selector
setFetchAttributesSelector = mkSelector "setFetchAttributes:"

-- | @Selector@ for @filterQueries@
filterQueriesSelector :: Selector
filterQueriesSelector = mkSelector "filterQueries"

-- | @Selector@ for @setFilterQueries:@
setFilterQueriesSelector :: Selector
setFilterQueriesSelector = mkSelector "setFilterQueries:"

-- | @Selector@ for @keyboardLanguage@
keyboardLanguageSelector :: Selector
keyboardLanguageSelector = mkSelector "keyboardLanguage"

-- | @Selector@ for @setKeyboardLanguage:@
setKeyboardLanguageSelector :: Selector
setKeyboardLanguageSelector = mkSelector "setKeyboardLanguage:"

-- | @Selector@ for @sourceOptions@
sourceOptionsSelector :: Selector
sourceOptionsSelector = mkSelector "sourceOptions"

-- | @Selector@ for @setSourceOptions:@
setSourceOptionsSelector :: Selector
setSourceOptionsSelector = mkSelector "setSourceOptions:"

