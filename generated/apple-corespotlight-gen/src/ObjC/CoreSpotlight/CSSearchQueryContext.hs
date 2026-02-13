{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , filterQueriesSelector
  , keyboardLanguageSelector
  , setFetchAttributesSelector
  , setFilterQueriesSelector
  , setKeyboardLanguageSelector
  , setSourceOptionsSelector
  , sourceOptionsSelector

  -- * Enum types
  , CSSearchQuerySourceOptions(CSSearchQuerySourceOptions)
  , pattern CSSearchQuerySourceOptionDefault
  , pattern CSSearchQuerySourceOptionAllowMail

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- fetchAttributes@
fetchAttributes :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSArray)
fetchAttributes csSearchQueryContext =
  sendMessage csSearchQueryContext fetchAttributesSelector

-- | @- setFetchAttributes:@
setFetchAttributes :: (IsCSSearchQueryContext csSearchQueryContext, IsNSArray value) => csSearchQueryContext -> value -> IO ()
setFetchAttributes csSearchQueryContext value =
  sendMessage csSearchQueryContext setFetchAttributesSelector (toNSArray value)

-- | @- filterQueries@
filterQueries :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSArray)
filterQueries csSearchQueryContext =
  sendMessage csSearchQueryContext filterQueriesSelector

-- | @- setFilterQueries:@
setFilterQueries :: (IsCSSearchQueryContext csSearchQueryContext, IsNSArray value) => csSearchQueryContext -> value -> IO ()
setFilterQueries csSearchQueryContext value =
  sendMessage csSearchQueryContext setFilterQueriesSelector (toNSArray value)

-- | @- keyboardLanguage@
keyboardLanguage :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO (Id NSString)
keyboardLanguage csSearchQueryContext =
  sendMessage csSearchQueryContext keyboardLanguageSelector

-- | @- setKeyboardLanguage:@
setKeyboardLanguage :: (IsCSSearchQueryContext csSearchQueryContext, IsNSString value) => csSearchQueryContext -> value -> IO ()
setKeyboardLanguage csSearchQueryContext value =
  sendMessage csSearchQueryContext setKeyboardLanguageSelector (toNSString value)

-- | @- sourceOptions@
sourceOptions :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> IO CSSearchQuerySourceOptions
sourceOptions csSearchQueryContext =
  sendMessage csSearchQueryContext sourceOptionsSelector

-- | @- setSourceOptions:@
setSourceOptions :: IsCSSearchQueryContext csSearchQueryContext => csSearchQueryContext -> CSSearchQuerySourceOptions -> IO ()
setSourceOptions csSearchQueryContext value =
  sendMessage csSearchQueryContext setSourceOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchAttributes@
fetchAttributesSelector :: Selector '[] (Id NSArray)
fetchAttributesSelector = mkSelector "fetchAttributes"

-- | @Selector@ for @setFetchAttributes:@
setFetchAttributesSelector :: Selector '[Id NSArray] ()
setFetchAttributesSelector = mkSelector "setFetchAttributes:"

-- | @Selector@ for @filterQueries@
filterQueriesSelector :: Selector '[] (Id NSArray)
filterQueriesSelector = mkSelector "filterQueries"

-- | @Selector@ for @setFilterQueries:@
setFilterQueriesSelector :: Selector '[Id NSArray] ()
setFilterQueriesSelector = mkSelector "setFilterQueries:"

-- | @Selector@ for @keyboardLanguage@
keyboardLanguageSelector :: Selector '[] (Id NSString)
keyboardLanguageSelector = mkSelector "keyboardLanguage"

-- | @Selector@ for @setKeyboardLanguage:@
setKeyboardLanguageSelector :: Selector '[Id NSString] ()
setKeyboardLanguageSelector = mkSelector "setKeyboardLanguage:"

-- | @Selector@ for @sourceOptions@
sourceOptionsSelector :: Selector '[] CSSearchQuerySourceOptions
sourceOptionsSelector = mkSelector "sourceOptions"

-- | @Selector@ for @setSourceOptions:@
setSourceOptionsSelector :: Selector '[CSSearchQuerySourceOptions] ()
setSourceOptionsSelector = mkSelector "setSourceOptions:"

