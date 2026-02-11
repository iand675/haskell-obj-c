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
  , registerLanguage_byVendorSelector
  , isWordInUserDictionaries_caseSensitiveSelector
  , runSelector


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

-- | @- registerLanguage:byVendor:@
registerLanguage_byVendor :: (IsNSSpellServer nsSpellServer, IsNSString language, IsNSString vendor) => nsSpellServer -> language -> vendor -> IO Bool
registerLanguage_byVendor nsSpellServer  language vendor =
withObjCPtr language $ \raw_language ->
  withObjCPtr vendor $ \raw_vendor ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellServer (mkSelector "registerLanguage:byVendor:") retCULong [argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_vendor :: Ptr ())]

-- | @- isWordInUserDictionaries:caseSensitive:@
isWordInUserDictionaries_caseSensitive :: (IsNSSpellServer nsSpellServer, IsNSString word) => nsSpellServer -> word -> Bool -> IO Bool
isWordInUserDictionaries_caseSensitive nsSpellServer  word flag =
withObjCPtr word $ \raw_word ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpellServer (mkSelector "isWordInUserDictionaries:caseSensitive:") retCULong [argPtr (castPtr raw_word :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- run@
run :: IsNSSpellServer nsSpellServer => nsSpellServer -> IO ()
run nsSpellServer  =
  sendMsg nsSpellServer (mkSelector "run") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerLanguage:byVendor:@
registerLanguage_byVendorSelector :: Selector
registerLanguage_byVendorSelector = mkSelector "registerLanguage:byVendor:"

-- | @Selector@ for @isWordInUserDictionaries:caseSensitive:@
isWordInUserDictionaries_caseSensitiveSelector :: Selector
isWordInUserDictionaries_caseSensitiveSelector = mkSelector "isWordInUserDictionaries:caseSensitive:"

-- | @Selector@ for @run@
runSelector :: Selector
runSelector = mkSelector "run"

