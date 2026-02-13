{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSpellServer@.
module ObjC.Foundation.NSSpellServer
  ( NSSpellServer
  , IsNSSpellServer(..)
  , registerLanguage_byVendor
  , isWordInUserDictionaries_caseSensitive
  , run
  , delegate
  , setDelegate
  , delegateSelector
  , isWordInUserDictionaries_caseSensitiveSelector
  , registerLanguage_byVendorSelector
  , runSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- registerLanguage:byVendor:@
registerLanguage_byVendor :: (IsNSSpellServer nsSpellServer, IsNSString language, IsNSString vendor) => nsSpellServer -> language -> vendor -> IO Bool
registerLanguage_byVendor nsSpellServer language vendor =
  sendMessage nsSpellServer registerLanguage_byVendorSelector (toNSString language) (toNSString vendor)

-- | @- isWordInUserDictionaries:caseSensitive:@
isWordInUserDictionaries_caseSensitive :: (IsNSSpellServer nsSpellServer, IsNSString word) => nsSpellServer -> word -> Bool -> IO Bool
isWordInUserDictionaries_caseSensitive nsSpellServer word flag =
  sendMessage nsSpellServer isWordInUserDictionaries_caseSensitiveSelector (toNSString word) flag

-- | @- run@
run :: IsNSSpellServer nsSpellServer => nsSpellServer -> IO ()
run nsSpellServer =
  sendMessage nsSpellServer runSelector

-- | @- delegate@
delegate :: IsNSSpellServer nsSpellServer => nsSpellServer -> IO RawId
delegate nsSpellServer =
  sendMessage nsSpellServer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSpellServer nsSpellServer => nsSpellServer -> RawId -> IO ()
setDelegate nsSpellServer value =
  sendMessage nsSpellServer setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerLanguage:byVendor:@
registerLanguage_byVendorSelector :: Selector '[Id NSString, Id NSString] Bool
registerLanguage_byVendorSelector = mkSelector "registerLanguage:byVendor:"

-- | @Selector@ for @isWordInUserDictionaries:caseSensitive:@
isWordInUserDictionaries_caseSensitiveSelector :: Selector '[Id NSString, Bool] Bool
isWordInUserDictionaries_caseSensitiveSelector = mkSelector "isWordInUserDictionaries:caseSensitive:"

-- | @Selector@ for @run@
runSelector :: Selector '[] ()
runSelector = mkSelector "run"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

