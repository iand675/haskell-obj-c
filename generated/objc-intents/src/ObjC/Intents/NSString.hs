{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.Intents.NSString
  ( NSString
  , IsNSString(..)
  , deferredLocalizedIntentsStringWithFormat
  , deferredLocalizedIntentsStringWithFormat_fromTable
  , deferredLocalizedIntentsStringWithFormat_fromTable_arguments
  , deferredLocalizedIntentsStringWithFormatSelector
  , deferredLocalizedIntentsStringWithFormat_fromTableSelector
  , deferredLocalizedIntentsStringWithFormat_fromTable_argumentsSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @+ deferredLocalizedIntentsStringWithFormat:@
deferredLocalizedIntentsStringWithFormat :: IsNSString format => format -> IO (Id NSString)
deferredLocalizedIntentsStringWithFormat format =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      sendClassMsg cls' (mkSelector "deferredLocalizedIntentsStringWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deferredLocalizedIntentsStringWithFormat:fromTable:@
deferredLocalizedIntentsStringWithFormat_fromTable :: (IsNSString format, IsNSString table) => format -> table -> IO (Id NSString)
deferredLocalizedIntentsStringWithFormat_fromTable format table =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr table $ \raw_table ->
        sendClassMsg cls' (mkSelector "deferredLocalizedIntentsStringWithFormat:fromTable:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_table :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deferredLocalizedIntentsStringWithFormat:fromTable:arguments:@
deferredLocalizedIntentsStringWithFormat_fromTable_arguments :: (IsNSString format, IsNSString table) => format -> table -> RawId -> IO (Id NSString)
deferredLocalizedIntentsStringWithFormat_fromTable_arguments format table arguments =
  do
    cls' <- getRequiredClass "NSString"
    withObjCPtr format $ \raw_format ->
      withObjCPtr table $ \raw_table ->
        sendClassMsg cls' (mkSelector "deferredLocalizedIntentsStringWithFormat:fromTable:arguments:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argPtr (castPtr raw_table :: Ptr ()), argPtr (castPtr (unRawId arguments) :: Ptr ())] >>= retainedObject . castPtr


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deferredLocalizedIntentsStringWithFormat:@
deferredLocalizedIntentsStringWithFormatSelector :: Selector
deferredLocalizedIntentsStringWithFormatSelector = mkSelector "deferredLocalizedIntentsStringWithFormat:"

-- | @Selector@ for @deferredLocalizedIntentsStringWithFormat:fromTable:@
deferredLocalizedIntentsStringWithFormat_fromTableSelector :: Selector
deferredLocalizedIntentsStringWithFormat_fromTableSelector = mkSelector "deferredLocalizedIntentsStringWithFormat:fromTable:"

-- | @Selector@ for @deferredLocalizedIntentsStringWithFormat:fromTable:arguments:@
deferredLocalizedIntentsStringWithFormat_fromTable_argumentsSelector :: Selector
deferredLocalizedIntentsStringWithFormat_fromTable_argumentsSelector = mkSelector "deferredLocalizedIntentsStringWithFormat:fromTable:arguments:"

