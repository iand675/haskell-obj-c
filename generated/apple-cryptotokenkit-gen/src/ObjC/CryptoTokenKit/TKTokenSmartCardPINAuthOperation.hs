{-# LANGUAGE DataKinds #-}
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
  , apduTemplateSelector
  , pinByteOffsetSelector
  , pinFormatSelector
  , pinSelector
  , setAPDUTemplateSelector
  , setPINByteOffsetSelector
  , setPINFormatSelector
  , setPINSelector
  , setSmartCardSelector
  , smartCardSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
pinFormat tkTokenSmartCardPINAuthOperation =
  sendMessage tkTokenSmartCardPINAuthOperation pinFormatSelector

-- | PIN formatting properties.
--
-- Note: The property is initialized with a default instance of TKSmartCardPINFormat.
--
-- ObjC selector: @- setPINFormat:@
setPINFormat :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsTKSmartCardPINFormat value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setPINFormat tkTokenSmartCardPINAuthOperation value =
  sendMessage tkTokenSmartCardPINAuthOperation setPINFormatSelector (toTKSmartCardPINFormat value)

-- | APDU template into which PIN gets filled in. If set to nil, the system will not attempt to authenticate by sending the formatted APDU to the SmartCard, but rather the token itself is expected to perform the authentication.  It is preferred to provide APDUTemplate if possible, because it allows using hardware PINPad for secure PIN entry (provided that the reader has one).
--
-- ObjC selector: @- APDUTemplate@
apduTemplate :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id NSData)
apduTemplate tkTokenSmartCardPINAuthOperation =
  sendMessage tkTokenSmartCardPINAuthOperation apduTemplateSelector

-- | APDU template into which PIN gets filled in. If set to nil, the system will not attempt to authenticate by sending the formatted APDU to the SmartCard, but rather the token itself is expected to perform the authentication.  It is preferred to provide APDUTemplate if possible, because it allows using hardware PINPad for secure PIN entry (provided that the reader has one).
--
-- ObjC selector: @- setAPDUTemplate:@
setAPDUTemplate :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsNSData value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setAPDUTemplate tkTokenSmartCardPINAuthOperation value =
  sendMessage tkTokenSmartCardPINAuthOperation setAPDUTemplateSelector (toNSData value)

-- | Offset in bytes within APDU template to mark the location for filling in the PIN.
--
-- ObjC selector: @- PINByteOffset@
pinByteOffset :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO CLong
pinByteOffset tkTokenSmartCardPINAuthOperation =
  sendMessage tkTokenSmartCardPINAuthOperation pinByteOffsetSelector

-- | Offset in bytes within APDU template to mark the location for filling in the PIN.
--
-- ObjC selector: @- setPINByteOffset:@
setPINByteOffset :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> CLong -> IO ()
setPINByteOffset tkTokenSmartCardPINAuthOperation value =
  sendMessage tkTokenSmartCardPINAuthOperation setPINByteOffsetSelector value

-- | TKSmartCard to which the formatted APDU gets sent in order to authenticate (used only if 'APDUTemplate' is set).
--
-- ObjC selector: @- smartCard@
smartCard :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id TKSmartCard)
smartCard tkTokenSmartCardPINAuthOperation =
  sendMessage tkTokenSmartCardPINAuthOperation smartCardSelector

-- | TKSmartCard to which the formatted APDU gets sent in order to authenticate (used only if 'APDUTemplate' is set).
--
-- ObjC selector: @- setSmartCard:@
setSmartCard :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsTKSmartCard value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setSmartCard tkTokenSmartCardPINAuthOperation value =
  sendMessage tkTokenSmartCardPINAuthOperation setSmartCardSelector (toTKSmartCard value)

-- | PIN value which will be set when 'finishWithError:' gets triggered.  Note that the PIN is not set in case that APDUTemplate was set.  In this case, PIN was already sent to the card using specified template.
--
-- ObjC selector: @- PIN@
pin :: IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation => tkTokenSmartCardPINAuthOperation -> IO (Id NSString)
pin tkTokenSmartCardPINAuthOperation =
  sendMessage tkTokenSmartCardPINAuthOperation pinSelector

-- | PIN value which will be set when 'finishWithError:' gets triggered.  Note that the PIN is not set in case that APDUTemplate was set.  In this case, PIN was already sent to the card using specified template.
--
-- ObjC selector: @- setPIN:@
setPIN :: (IsTKTokenSmartCardPINAuthOperation tkTokenSmartCardPINAuthOperation, IsNSString value) => tkTokenSmartCardPINAuthOperation -> value -> IO ()
setPIN tkTokenSmartCardPINAuthOperation value =
  sendMessage tkTokenSmartCardPINAuthOperation setPINSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PINFormat@
pinFormatSelector :: Selector '[] (Id TKSmartCardPINFormat)
pinFormatSelector = mkSelector "PINFormat"

-- | @Selector@ for @setPINFormat:@
setPINFormatSelector :: Selector '[Id TKSmartCardPINFormat] ()
setPINFormatSelector = mkSelector "setPINFormat:"

-- | @Selector@ for @APDUTemplate@
apduTemplateSelector :: Selector '[] (Id NSData)
apduTemplateSelector = mkSelector "APDUTemplate"

-- | @Selector@ for @setAPDUTemplate:@
setAPDUTemplateSelector :: Selector '[Id NSData] ()
setAPDUTemplateSelector = mkSelector "setAPDUTemplate:"

-- | @Selector@ for @PINByteOffset@
pinByteOffsetSelector :: Selector '[] CLong
pinByteOffsetSelector = mkSelector "PINByteOffset"

-- | @Selector@ for @setPINByteOffset:@
setPINByteOffsetSelector :: Selector '[CLong] ()
setPINByteOffsetSelector = mkSelector "setPINByteOffset:"

-- | @Selector@ for @smartCard@
smartCardSelector :: Selector '[] (Id TKSmartCard)
smartCardSelector = mkSelector "smartCard"

-- | @Selector@ for @setSmartCard:@
setSmartCardSelector :: Selector '[Id TKSmartCard] ()
setSmartCardSelector = mkSelector "setSmartCard:"

-- | @Selector@ for @PIN@
pinSelector :: Selector '[] (Id NSString)
pinSelector = mkSelector "PIN"

-- | @Selector@ for @setPIN:@
setPINSelector :: Selector '[Id NSString] ()
setPINSelector = mkSelector "setPIN:"

