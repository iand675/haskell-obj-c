{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that matches a specific audio recording when a segment of that recording is part of captured sound in the Shazam catalog or your custom catalog.
--
-- Prepare to make matches by:
--
-- - Creating a session for the catalog that contains the reference signatures - Adding your delegate that receives the match results
--
-- Search for a match in one of two ways:
--
-- - Generate a signature for the captured audio and call ``match(_:)`` - Call ``matchStreamingBuffer(_:at:)`` with a streaming audio buffer, and ShazamKit generates the signature for you
--
-- Searching the catalog is asynchronous. The session calls your delegate methods with the result.
--
-- Matching songs in Shazam music requires enabling your app to access the catalog. For more information on enabling your app, see [Enable ShazamKit for an App ID](https://developer.apple.com/help/account/configure-app-services/shazamkit).
--
-- The code below shows searching for a match in the Shazam catalog using an existing audio buffer:
--
-- ```swift// Set up the session.let session = SHSession()
--
-- // Create a signature from the captured audio buffer.let signatureGenerator = SHSignatureGenerator()try signatureGenerator.append(buffer, at: audioTime)let signature = signatureGenerator.signature()
--
-- // Check for a match.let result = await session.result(from: signature)
--
-- // Use the result.switch result {  case .match(let match):       // Match found.  case .noMatch(let signature):       // No match found.  case .error(let error, let signature):       // An error occurred.} ```
--
-- Generated bindings for @SHSession@.
module ObjC.ShazamKit.SHSession
  ( SHSession
  , IsSHSession(..)
  , init_
  , initWithCatalog
  , matchStreamingBuffer_atTime
  , matchSignature
  , catalog
  , initSelector
  , initWithCatalogSelector
  , matchStreamingBuffer_atTimeSelector
  , matchSignatureSelector
  , catalogSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new session object for matching songs in the Shazam Music catalog.
--
-- ObjC selector: @- init@
init_ :: IsSHSession shSession => shSession -> IO (Id SHSession)
init_ shSession  =
  sendMsg shSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a new session object for matching audio in a custom catalog.
--
-- - Parameters:     - catalog: The catalog that contains the reference audio signatures and their associated metadata.
--
-- ObjC selector: @- initWithCatalog:@
initWithCatalog :: (IsSHSession shSession, IsSHCatalog catalog) => shSession -> catalog -> IO (Id SHSession)
initWithCatalog shSession  catalog =
withObjCPtr catalog $ \raw_catalog ->
    sendMsg shSession (mkSelector "initWithCatalog:") (retPtr retVoid) [argPtr (castPtr raw_catalog :: Ptr ())] >>= ownedObject . castPtr

-- | Converts the audio in the buffer to a signature, and searches the reference signatures in the session catalog.
--
-- This method continues to generate signatures and perform searches until the audio in the buffer stops, which may result in multiple calls to the ``SHSession/delegate``.
--
-- The audio buffer must be in one of the supported formats. For the list of the supported audio formats, see ``SHSignatureGenerator/append(_:at:)``.
--
-- To use the microphone as input for the buffer, see <doc:matching-audio-using-the-built-in-microphone>.
--
-- > Note: > You must use the audio format of the first call to this method in the current session in all subsequent calls for the session.
--
-- - Parameters:    - buffer: An audio buffer.    - time: The start time of the audio to use for generating the signatures.
--
-- ObjC selector: @- matchStreamingBuffer:atTime:@
matchStreamingBuffer_atTime :: (IsSHSession shSession, IsAVAudioPCMBuffer buffer, IsAVAudioTime time) => shSession -> buffer -> time -> IO ()
matchStreamingBuffer_atTime shSession  buffer time =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr time $ \raw_time ->
      sendMsg shSession (mkSelector "matchStreamingBuffer:atTime:") retVoid [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_time :: Ptr ())]

-- | Searches for the query signature in the reference signatures that the session catalog contains.
--
-- - Parameters:  - signature: The signature for searching the catalog of reference signatures.
--
-- ObjC selector: @- matchSignature:@
matchSignature :: (IsSHSession shSession, IsSHSignature signature) => shSession -> signature -> IO ()
matchSignature shSession  signature =
withObjCPtr signature $ \raw_signature ->
    sendMsg shSession (mkSelector "matchSignature:") retVoid [argPtr (castPtr raw_signature :: Ptr ())]

-- | The catalog object containing the reference signatures and their associated metadata that the session uses to perform matches.
--
-- ObjC selector: @- catalog@
catalog :: IsSHSession shSession => shSession -> IO (Id SHCatalog)
catalog shSession  =
  sendMsg shSession (mkSelector "catalog") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCatalog:@
initWithCatalogSelector :: Selector
initWithCatalogSelector = mkSelector "initWithCatalog:"

-- | @Selector@ for @matchStreamingBuffer:atTime:@
matchStreamingBuffer_atTimeSelector :: Selector
matchStreamingBuffer_atTimeSelector = mkSelector "matchStreamingBuffer:atTime:"

-- | @Selector@ for @matchSignature:@
matchSignatureSelector :: Selector
matchSignatureSelector = mkSelector "matchSignature:"

-- | @Selector@ for @catalog@
catalogSelector :: Selector
catalogSelector = mkSelector "catalog"

