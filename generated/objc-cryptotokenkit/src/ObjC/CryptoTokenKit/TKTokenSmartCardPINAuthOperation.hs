{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Context of a SmartCard PIN authentication operation.
--
-- Generated bindings for @TKTokenSmartCardPINAuthOperation@.
module ObjC.CryptoTokenKit.TKTokenSmartCardPINAuthOperation
  ( TKTokenSmartCardPINAuthOperation
  , IsTKTokenSmartCardPINAuthOperation(..)
  , pinFormat
  , setPINFormat
  , apduTemplate
  , setAPDUTemplate
  , pinByteOffset
  , setPINByteOffset
  , smartCard
  , setSmartCard
  , pin
  , setPIN
  , pinFormatSelector
  , setPINFormatSelector
  , apduTemplateSelector
  , setAPDUTemplateSelector
  , pinByteOffsetSelector
  , setPINByteOffsetSelector
  , smartCardSelector
  , setSmartCardSelector
  , pinSelector
  , setPINSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | PIN formatting properties.
--
-- Note: The property is initialized with a default instance of TKSmartCardPINFormat.
--
-- ObjC selector: @- PINFormat@
pinFormat :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id TKSmartCardPINFormat)
pinFormat tkTokenSmartCardPINAuthOperation  =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "PINFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | PIN formatting properties.
--
-- Note: The property is initialized with a default instance of TKSmartCardPINFormat.
--
-- ObjC selector: @- setPINFormat:@
setPINFormat :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsTKSmartCardPINFormat value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setPINFormat tkTokenSmartCardPINAuthOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "setPINFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | APDU template into which PIN gets filled in. If set to nil, the system will not attempt to authenticate by sending the formatted APDU to the SmartCard, but rather the token itself is expected to perform the authentication.  It is preferred to provide APDUTemplate if possible, because it allows using hardware PINPad for secure PIN entry (provided that the reader has one).
--
-- ObjC selector: @- APDUTemplate@
apduTemplate :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id NSData)
apduTemplate tkTokenSmartCardPINAuthOperation  =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "APDUTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | APDU template into which PIN gets filled in. If set to nil, the system will not attempt to authenticate by sending the formatted APDU to the SmartCard, but rather the token itself is expected to perform the authentication.  It is preferred to provide APDUTemplate if possible, because it allows using hardware PINPad for secure PIN entry (provided that the reader has one).
--
-- ObjC selector: @- setAPDUTemplate:@
setAPDUTemplate :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsNSData value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setAPDUTemplate tkTokenSmartCardPINAuthOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "setAPDUTemplate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Offset in bytes within APDU template to mark the location for filling in the PIN.
--
-- ObjC selector: @- PINByteOffset@
pinByteOffset :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO CLong
pinByteOffset tkTokenSmartCardPINAuthOperation  =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "PINByteOffset") retCLong []

-- | Offset in bytes within APDU template to mark the location for filling in the PIN.
--
-- ObjC selector: @- setPINByteOffset:@
setPINByteOffset :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> CLong -> IO ()
setPINByteOffset tkTokenSmartCardPINAuthOperation  value =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "setPINByteOffset:") retVoid [argCLong (fromIntegral value)]

-- | TKSmartCard to which the formatted APDU gets sent in order to authenticate (used only if 'APDUTemplate' is set).
--
-- ObjC selector: @- smartCard@
smartCard :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id TKSmartCard)
smartCard tkTokenSmartCardPINAuthOperation  =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "smartCard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | TKSmartCard to which the formatted APDU gets sent in order to authenticate (used only if 'APDUTemplate' is set).
--
-- ObjC selector: @- setSmartCard:@
setSmartCard :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsTKSmartCard value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setSmartCard tkTokenSmartCardPINAuthOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "setSmartCard:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | PIN value which will be set when 'finishWithError:' gets triggered.  Note that the PIN is not set in case that APDUTemplate was set.  In this case, PIN was already sent to the card using specified template.
--
-- ObjC selector: @- PIN@
pin :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id NSString)
pin tkTokenSmartCardPINAuthOperation  =
  sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "PIN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | PIN value which will be set when 'finishWithError:' gets triggered.  Note that the PIN is not set in case that APDUTemplate was set.  In this case, PIN was already sent to the card using specified template.
--
-- ObjC selector: @- setPIN:@
setPIN :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsNSString value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setPIN tkTokenSmartCardPINAuthOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenSmartCardPINAuthOperation (mkSelector "setPIN:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINFormat@
pinFormatSelector :: Selector
pinFormatSelector = mkSelector "PINFormat"

-- | @Selector@ for @setPINFormat:@
setPINFormatSelector :: Selector
setPINFormatSelector = mkSelector "setPINFormat:"

-- | @Selector@ for @APDUTemplate@
apduTemplateSelector :: Selector
apduTemplateSelector = mkSelector "APDUTemplate"

-- | @Selector@ for @setAPDUTemplate:@
setAPDUTemplateSelector :: Selector
setAPDUTemplateSelector = mkSelector "setAPDUTemplate:"

-- | @Selector@ for @PINByteOffset@
pinByteOffsetSelector :: Selector
pinByteOffsetSelector = mkSelector "PINByteOffset"

-- | @Selector@ for @setPINByteOffset:@
setPINByteOffsetSelector :: Selector
setPINByteOffsetSelector = mkSelector "setPINByteOffset:"

-- | @Selector@ for @smartCard@
smartCardSelector :: Selector
smartCardSelector = mkSelector "smartCard"

-- | @Selector@ for @setSmartCard:@
setSmartCardSelector :: Selector
setSmartCardSelector = mkSelector "setSmartCard:"

-- | @Selector@ for @PIN@
pinSelector :: Selector
pinSelector = mkSelector "PIN"

-- | @Selector@ for @setPIN:@
setPINSelector :: Selector
setPINSelector = mkSelector "setPIN:"

