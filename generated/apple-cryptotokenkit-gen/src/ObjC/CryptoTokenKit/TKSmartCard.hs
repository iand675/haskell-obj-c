{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents SmartCard inserted in the slot. Once the card is physically removed from the slot, the session object is invalid and will always fail the operation invoked on it.  In order to communicate with the card, an exclusive session must be established.
--
-- Generated bindings for @TKSmartCard@.
module ObjC.CryptoTokenKit.TKSmartCard
  ( TKSmartCard
  , IsTKSmartCard(..)
  , beginSessionWithReply
  , transmitRequest_reply
  , endSession
  , userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffset
  , userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffset
  , sendIns_p1_p2_data_le_reply
  , inSessionWithError_executeBlock
  , sendIns_p1_p2_data_le_sw_error
  , slot
  , valid
  , allowedProtocols
  , setAllowedProtocols
  , currentProtocol
  , sensitive
  , setSensitive
  , context
  , setContext
  , cla
  , setCla
  , useExtendedLength
  , setUseExtendedLength
  , useCommandChaining
  , setUseCommandChaining
  , allowedProtocolsSelector
  , beginSessionWithReplySelector
  , claSelector
  , contextSelector
  , currentProtocolSelector
  , endSessionSelector
  , inSessionWithError_executeBlockSelector
  , sendIns_p1_p2_data_le_replySelector
  , sendIns_p1_p2_data_le_sw_errorSelector
  , sensitiveSelector
  , setAllowedProtocolsSelector
  , setClaSelector
  , setContextSelector
  , setSensitiveSelector
  , setUseCommandChainingSelector
  , setUseExtendedLengthSelector
  , slotSelector
  , transmitRequest_replySelector
  , useCommandChainingSelector
  , useExtendedLengthSelector
  , userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffsetSelector
  , userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffsetSelector
  , validSelector

  -- * Enum types
  , TKSmartCardProtocol(TKSmartCardProtocol)
  , pattern TKSmartCardProtocolNone
  , pattern TKSmartCardProtocolT0
  , pattern TKSmartCardProtocolT1
  , pattern TKSmartCardProtocolT15
  , pattern TKSmartCardProtocolAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.CryptoTokenKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Begins session with the card.
--
-- When session exists, other requests for sessions from other card objects to the same card are blocked. Session is reference-counted, the same amount of 'end' calls must be done to really terminate the session. Note that finishing session does not automatically mean that the card is disconnected; it only happens when another session from different card object is requested.
--
-- @success@ — Signals whether session was successfully started.
--
-- @error@ — More information about error preventing the transaction to start
--
-- ObjC selector: @- beginSessionWithReply:@
beginSessionWithReply :: IsTKSmartCard tkSmartCard => tkSmartCard -> Ptr () -> IO ()
beginSessionWithReply tkSmartCard reply =
  sendMessage tkSmartCard beginSessionWithReplySelector reply

-- | Transmits raw command to the card.  This call is allowed only inside session.
--
-- @request@ — Request part of APDU
--
-- @reponse@ — Response part of APDU, or nil if communication with the card failed
--
-- @error@ — Error details when communication with the card failed
--
-- ObjC selector: @- transmitRequest:reply:@
transmitRequest_reply :: (IsTKSmartCard tkSmartCard, IsNSData request) => tkSmartCard -> request -> Ptr () -> IO ()
transmitRequest_reply tkSmartCard request reply =
  sendMessage tkSmartCard transmitRequest_replySelector (toNSData request) reply

-- | Terminates the transaction. If no transaction is pending any more, the connection will be closed if there is another session in the system waiting for the transaction.
--
-- ObjC selector: @- endSession@
endSession :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO ()
endSession tkSmartCard =
  sendMessage tkSmartCard endSessionSelector

-- | Creates a new user interaction object for secure PIN verification using the SmartCard reader facilities (typically a HW keypad).
--
-- Note: This interaction is only allowed within a session.
--
-- @PINFormat@ — PIN format descriptor.
--
-- @APDU@ — Predefined APDU in which the SmartCard reader fills in the PIN.
--
-- @PINByteOffset@ — Offset in bytes within APDU data field to mark a location of a PIN block for filling in the entered PIN (currently unused, must be 0).
--
-- Returns: A new user interaction object, or nil if this feature is not supported by the SmartCard reader. After the interaction has been successfully completed the operation result is available in the result properites.
--
-- ObjC selector: @- userInteractionForSecurePINVerificationWithPINFormat:APDU:PINByteOffset:@
userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffset :: (IsTKSmartCard tkSmartCard, IsTKSmartCardPINFormat pinFormat, IsNSData apdu) => tkSmartCard -> pinFormat -> apdu -> CLong -> IO (Id TKSmartCardUserInteractionForSecurePINVerification)
userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffset tkSmartCard pinFormat apdu pinByteOffset =
  sendMessage tkSmartCard userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffsetSelector (toTKSmartCardPINFormat pinFormat) (toNSData apdu) pinByteOffset

-- | Creates a new user interaction object for secure PIN change using the SmartCard reader facilities (typically a HW keypad).
--
-- Note: This interaction is only allowed within a session.
--
-- @PINFormat@ — PIN format descriptor.
--
-- @APDU@ — Predefined APDU in which the SmartCard reader fills in the PIN(s).
--
-- @currentPINByteOffset@ — Offset in bytes within APDU data field to mark a location of a PIN block for filling in the current PIN.
--
-- @newPINByteOffset@ — Offset in bytes within APDU data field to mark a location of a PIN block for filling in the new PIN.
--
-- Returns: A new user interaction object, or nil if this feature is not supported by the SmartCard reader. After the interaction has been successfully completed the operation result is available in the result properites.
--
-- ObjC selector: @- userInteractionForSecurePINChangeWithPINFormat:APDU:currentPINByteOffset:newPINByteOffset:@
userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffset :: (IsTKSmartCard tkSmartCard, IsTKSmartCardPINFormat pinFormat, IsNSData apdu) => tkSmartCard -> pinFormat -> apdu -> CLong -> CLong -> IO (Id TKSmartCardUserInteractionForSecurePINChange)
userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffset tkSmartCard pinFormat apdu currentPINByteOffset newPINByteOffset =
  sendMessage tkSmartCard userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffsetSelector (toTKSmartCardPINFormat pinFormat) (toNSData apdu) currentPINByteOffset newPINByteOffset

-- | Transmits APDU to the card and returns response.
--
-- Asynchronous high level variant of command for transmitting APDU to the card.  Handles all ISO7816-4 APDU cases translation to proper sequences according to used protocol.  Consults useExtendedAPDU and useCommandChaining properties and uses these modes whenever appropriate and beneficial for sending requested APDU request.
--
-- @ins@ — INS code of the APDU
--
-- @p1@ — P1 code of the APDU
--
-- @p2@ — P2 code of the APDU
--
-- @requestData@ — Data field of the APDU, or nil if no input data field should be present (i.e case1 or case2 APDUs).  Length of the data serves as Lc field of the APDU.
--
-- @le@ — Expected number of bytes to be returned, or nil if no output data are expected (i.e. case1 or case3 APDUs). To get as much bytes as card provides, pass \@0.
--
-- @replyData@ — Block of returned data without SW1SW2 bytes, or nil if an error occured.
--
-- @sw@ — SW1SW2 result code, first two bytes of returned card's reply.
--
-- @error@ — Contains error details when nil is returned.  Specific error is also filled in if there was no communication error, but card returned other SW code than 0x9000.
--
-- ObjC selector: @- sendIns:p1:p2:data:le:reply:@
sendIns_p1_p2_data_le_reply :: (IsTKSmartCard tkSmartCard, IsNSData requestData, IsNSNumber le) => tkSmartCard -> CUChar -> CUChar -> CUChar -> requestData -> le -> Ptr () -> IO ()
sendIns_p1_p2_data_le_reply tkSmartCard ins p1 p2 requestData le reply =
  sendMessage tkSmartCard sendIns_p1_p2_data_le_replySelector ins p1 p2 (toNSData requestData) (toNSNumber le) reply

-- | Synchronous variant of session creation.  Begins the session, executes given block and ends session.
--
-- @error@ — Error receiving more information when transaction failed to start or block failed for some reason.
--
-- @block@ — Block to be executed when the session was successfully begun.
--
-- Returns: Returns YES if the session was successfully begun and block returned YES, otherwise NO.
--
-- ObjC selector: @- inSessionWithError:executeBlock:@
inSessionWithError_executeBlock :: (IsTKSmartCard tkSmartCard, IsNSError error_) => tkSmartCard -> error_ -> Ptr () -> IO Bool
inSessionWithError_executeBlock tkSmartCard error_ block =
  sendMessage tkSmartCard inSessionWithError_executeBlockSelector (toNSError error_) block

-- | Transmits APDU to the card and returns response.
--
-- Synchronous high level variant of command for transmitting APDU to the card.  Handles all ISO7816-4 APDU cases translation to proper sequences according to used protocol.  Should be used in block passed to -[TKSmartCard inSessionWithError:executeBlock:] method.
--
-- @ins@ — INS code of the APDU
--
-- @p1@ — P1 code of the APDU
--
-- @p2@ — P2 code of the APDU
--
-- @data@ — Data field of the APDU.  Length of the data serves as Lc field of the APDU
--
-- @le@ — Expected number of bytes to be returned, or nil if no output data are expected (i.e. case1 or case3 APDUs). To get as much bytes as card provides, pass \@0.
--
-- @sw@ — On output, filled with SW1SW2 result code
--
-- @error@ — Contains error details when nil is returned.  Specific error is also filled in if there was no communication error, but card returned other SW code than 0x9000.
--
-- Returns: Returned data field, excluding SW status bytes.  If an error occured, returns nil.
--
-- ObjC selector: @- sendIns:p1:p2:data:le:sw:error:@
sendIns_p1_p2_data_le_sw_error :: (IsTKSmartCard tkSmartCard, IsNSData requestData, IsNSNumber le, IsNSError error_) => tkSmartCard -> CUChar -> CUChar -> CUChar -> requestData -> le -> RawId -> error_ -> IO (Id NSData)
sendIns_p1_p2_data_le_sw_error tkSmartCard ins p1 p2 requestData le sw error_ =
  sendMessage tkSmartCard sendIns_p1_p2_data_le_sw_errorSelector ins p1 p2 (toNSData requestData) (toNSNumber le) sw (toNSError error_)

-- | Slot in which is this card inserted.
--
-- ObjC selector: @- slot@
slot :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO (Id TKSmartCardSlot)
slot tkSmartCard =
  sendMessage tkSmartCard slotSelector

-- | Flag indicating whether card is valid, i.e. it was not removed from the reader.  Use Key-Value-Observing to be notified about card removal.
--
-- ObjC selector: @- valid@
valid :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO Bool
valid tkSmartCard =
  sendMessage tkSmartCard validSelector

-- | Bitmask containing allowed protocols to be used when communicating with the card.  This property is consulted only during connection to the card, changes are not propagated to already connected session.  By default, any protocol can be used.
--
-- ObjC selector: @- allowedProtocols@
allowedProtocols :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO TKSmartCardProtocol
allowedProtocols tkSmartCard =
  sendMessage tkSmartCard allowedProtocolsSelector

-- | Bitmask containing allowed protocols to be used when communicating with the card.  This property is consulted only during connection to the card, changes are not propagated to already connected session.  By default, any protocol can be used.
--
-- ObjC selector: @- setAllowedProtocols:@
setAllowedProtocols :: IsTKSmartCard tkSmartCard => tkSmartCard -> TKSmartCardProtocol -> IO ()
setAllowedProtocols tkSmartCard value =
  sendMessage tkSmartCard setAllowedProtocolsSelector value

-- | Protocol used for communication with the SmartCard.  If no card session is established, TKSmartCardProtocolNone is set.
--
-- ObjC selector: @- currentProtocol@
currentProtocol :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO TKSmartCardProtocol
currentProtocol tkSmartCard =
  sendMessage tkSmartCard currentProtocolSelector

-- | Flag indicating whether card session should be considered as sensitive.  Sensitive session always gets card after reset before communicating with it and never leaves card without reset to be used by another SmartCard object.  This might be important in case that card session contain some important state which should not leak to another SmartCard object (possibly running in another, foreign application).  Default is NO.
--
-- ObjC selector: @- sensitive@
sensitive :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO Bool
sensitive tkSmartCard =
  sendMessage tkSmartCard sensitiveSelector

-- | Flag indicating whether card session should be considered as sensitive.  Sensitive session always gets card after reset before communicating with it and never leaves card without reset to be used by another SmartCard object.  This might be important in case that card session contain some important state which should not leak to another SmartCard object (possibly running in another, foreign application).  Default is NO.
--
-- ObjC selector: @- setSensitive:@
setSensitive :: IsTKSmartCard tkSmartCard => tkSmartCard -> Bool -> IO ()
setSensitive tkSmartCard value =
  sendMessage tkSmartCard setSensitiveSelector value

-- | User-specified context kept as long as the card is powered.  Once the card is removed or another TKSmartCard object opens session, this property is automatically set to nil.
--
-- ObjC selector: @- context@
context :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO RawId
context tkSmartCard =
  sendMessage tkSmartCard contextSelector

-- | User-specified context kept as long as the card is powered.  Once the card is removed or another TKSmartCard object opens session, this property is automatically set to nil.
--
-- ObjC selector: @- setContext:@
setContext :: IsTKSmartCard tkSmartCard => tkSmartCard -> RawId -> IO ()
setContext tkSmartCard value =
  sendMessage tkSmartCard setContextSelector value

-- | CLA byte which will be used for sendIns: APDU transmits.  Default value is 0x00.
--
-- ObjC selector: @- cla@
cla :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO CUChar
cla tkSmartCard =
  sendMessage tkSmartCard claSelector

-- | CLA byte which will be used for sendIns: APDU transmits.  Default value is 0x00.
--
-- ObjC selector: @- setCla:@
setCla :: IsTKSmartCard tkSmartCard => tkSmartCard -> CUChar -> IO ()
setCla tkSmartCard value =
  sendMessage tkSmartCard setClaSelector value

-- | Flag indicating whether extended length APDUs should be used. It is automatically enabled only when used slot supports transmitting extended length commands and card announces that extended length APDU are supported in its ATR. However, caller can explicitly override this decision.
--
-- ObjC selector: @- useExtendedLength@
useExtendedLength :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO Bool
useExtendedLength tkSmartCard =
  sendMessage tkSmartCard useExtendedLengthSelector

-- | Flag indicating whether extended length APDUs should be used. It is automatically enabled only when used slot supports transmitting extended length commands and card announces that extended length APDU are supported in its ATR. However, caller can explicitly override this decision.
--
-- ObjC selector: @- setUseExtendedLength:@
setUseExtendedLength :: IsTKSmartCard tkSmartCard => tkSmartCard -> Bool -> IO ()
setUseExtendedLength tkSmartCard value =
  sendMessage tkSmartCard setUseExtendedLengthSelector value

-- | Flag indicating whether command chaining of APDU with data field longer than 255 bytes can be used.  It is automatically enabled when card announces that command chaining is supported in its ATR.  However, caller can explicitly override this decision.
--
-- ObjC selector: @- useCommandChaining@
useCommandChaining :: IsTKSmartCard tkSmartCard => tkSmartCard -> IO Bool
useCommandChaining tkSmartCard =
  sendMessage tkSmartCard useCommandChainingSelector

-- | Flag indicating whether command chaining of APDU with data field longer than 255 bytes can be used.  It is automatically enabled when card announces that command chaining is supported in its ATR.  However, caller can explicitly override this decision.
--
-- ObjC selector: @- setUseCommandChaining:@
setUseCommandChaining :: IsTKSmartCard tkSmartCard => tkSmartCard -> Bool -> IO ()
setUseCommandChaining tkSmartCard value =
  sendMessage tkSmartCard setUseCommandChainingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginSessionWithReply:@
beginSessionWithReplySelector :: Selector '[Ptr ()] ()
beginSessionWithReplySelector = mkSelector "beginSessionWithReply:"

-- | @Selector@ for @transmitRequest:reply:@
transmitRequest_replySelector :: Selector '[Id NSData, Ptr ()] ()
transmitRequest_replySelector = mkSelector "transmitRequest:reply:"

-- | @Selector@ for @endSession@
endSessionSelector :: Selector '[] ()
endSessionSelector = mkSelector "endSession"

-- | @Selector@ for @userInteractionForSecurePINVerificationWithPINFormat:APDU:PINByteOffset:@
userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffsetSelector :: Selector '[Id TKSmartCardPINFormat, Id NSData, CLong] (Id TKSmartCardUserInteractionForSecurePINVerification)
userInteractionForSecurePINVerificationWithPINFormat_APDU_PINByteOffsetSelector = mkSelector "userInteractionForSecurePINVerificationWithPINFormat:APDU:PINByteOffset:"

-- | @Selector@ for @userInteractionForSecurePINChangeWithPINFormat:APDU:currentPINByteOffset:newPINByteOffset:@
userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffsetSelector :: Selector '[Id TKSmartCardPINFormat, Id NSData, CLong, CLong] (Id TKSmartCardUserInteractionForSecurePINChange)
userInteractionForSecurePINChangeWithPINFormat_APDU_currentPINByteOffset_newPINByteOffsetSelector = mkSelector "userInteractionForSecurePINChangeWithPINFormat:APDU:currentPINByteOffset:newPINByteOffset:"

-- | @Selector@ for @sendIns:p1:p2:data:le:reply:@
sendIns_p1_p2_data_le_replySelector :: Selector '[CUChar, CUChar, CUChar, Id NSData, Id NSNumber, Ptr ()] ()
sendIns_p1_p2_data_le_replySelector = mkSelector "sendIns:p1:p2:data:le:reply:"

-- | @Selector@ for @inSessionWithError:executeBlock:@
inSessionWithError_executeBlockSelector :: Selector '[Id NSError, Ptr ()] Bool
inSessionWithError_executeBlockSelector = mkSelector "inSessionWithError:executeBlock:"

-- | @Selector@ for @sendIns:p1:p2:data:le:sw:error:@
sendIns_p1_p2_data_le_sw_errorSelector :: Selector '[CUChar, CUChar, CUChar, Id NSData, Id NSNumber, RawId, Id NSError] (Id NSData)
sendIns_p1_p2_data_le_sw_errorSelector = mkSelector "sendIns:p1:p2:data:le:sw:error:"

-- | @Selector@ for @slot@
slotSelector :: Selector '[] (Id TKSmartCardSlot)
slotSelector = mkSelector "slot"

-- | @Selector@ for @valid@
validSelector :: Selector '[] Bool
validSelector = mkSelector "valid"

-- | @Selector@ for @allowedProtocols@
allowedProtocolsSelector :: Selector '[] TKSmartCardProtocol
allowedProtocolsSelector = mkSelector "allowedProtocols"

-- | @Selector@ for @setAllowedProtocols:@
setAllowedProtocolsSelector :: Selector '[TKSmartCardProtocol] ()
setAllowedProtocolsSelector = mkSelector "setAllowedProtocols:"

-- | @Selector@ for @currentProtocol@
currentProtocolSelector :: Selector '[] TKSmartCardProtocol
currentProtocolSelector = mkSelector "currentProtocol"

-- | @Selector@ for @sensitive@
sensitiveSelector :: Selector '[] Bool
sensitiveSelector = mkSelector "sensitive"

-- | @Selector@ for @setSensitive:@
setSensitiveSelector :: Selector '[Bool] ()
setSensitiveSelector = mkSelector "setSensitive:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] RawId
contextSelector = mkSelector "context"

-- | @Selector@ for @setContext:@
setContextSelector :: Selector '[RawId] ()
setContextSelector = mkSelector "setContext:"

-- | @Selector@ for @cla@
claSelector :: Selector '[] CUChar
claSelector = mkSelector "cla"

-- | @Selector@ for @setCla:@
setClaSelector :: Selector '[CUChar] ()
setClaSelector = mkSelector "setCla:"

-- | @Selector@ for @useExtendedLength@
useExtendedLengthSelector :: Selector '[] Bool
useExtendedLengthSelector = mkSelector "useExtendedLength"

-- | @Selector@ for @setUseExtendedLength:@
setUseExtendedLengthSelector :: Selector '[Bool] ()
setUseExtendedLengthSelector = mkSelector "setUseExtendedLength:"

-- | @Selector@ for @useCommandChaining@
useCommandChainingSelector :: Selector '[] Bool
useCommandChainingSelector = mkSelector "useCommandChaining"

-- | @Selector@ for @setUseCommandChaining:@
setUseCommandChainingSelector :: Selector '[Bool] ()
setUseCommandChainingSelector = mkSelector "setUseCommandChaining:"

