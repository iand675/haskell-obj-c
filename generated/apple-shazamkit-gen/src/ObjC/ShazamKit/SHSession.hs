{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , catalogSelector
  , delegateSelector
  , initSelector
  , initWithCatalogSelector
  , matchSignatureSelector
  , matchStreamingBuffer_atTimeSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ShazamKit.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new session object for matching songs in the Shazam Music catalog.
--
-- ObjC selector: @- init@
init_ :: IsSHSession shSession => shSession -> IO (Id SHSession)
init_ shSession =
  sendOwnedMessage shSession initSelector

-- | Creates a new session object for matching audio in a custom catalog.
--
-- - Parameters:     - catalog: The catalog that contains the reference audio signatures and their associated metadata.
--
-- ObjC selector: @- initWithCatalog:@
initWithCatalog :: (IsSHSession shSession, IsSHCatalog catalog) => shSession -> catalog -> IO (Id SHSession)
initWithCatalog shSession catalog =
  sendOwnedMessage shSession initWithCatalogSelector (toSHCatalog catalog)

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
matchStreamingBuffer_atTime shSession buffer time =
  sendMessage shSession matchStreamingBuffer_atTimeSelector (toAVAudioPCMBuffer buffer) (toAVAudioTime time)

-- | Searches for the query signature in the reference signatures that the session catalog contains.
--
-- - Parameters:  - signature: The signature for searching the catalog of reference signatures.
--
-- ObjC selector: @- matchSignature:@
matchSignature :: (IsSHSession shSession, IsSHSignature signature) => shSession -> signature -> IO ()
matchSignature shSession signature =
  sendMessage shSession matchSignatureSelector (toSHSignature signature)

-- | The catalog object containing the reference signatures and their associated metadata that the session uses to perform matches.
--
-- ObjC selector: @- catalog@
catalog :: IsSHSession shSession => shSession -> IO (Id SHCatalog)
catalog shSession =
  sendMessage shSession catalogSelector

-- | The object that the session calls with the result of a match request.
--
-- ObjC selector: @- delegate@
delegate :: IsSHSession shSession => shSession -> IO RawId
delegate shSession =
  sendMessage shSession delegateSelector

-- | The object that the session calls with the result of a match request.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSHSession shSession => shSession -> RawId -> IO ()
setDelegate shSession value =
  sendMessage shSession setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SHSession)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCatalog:@
initWithCatalogSelector :: Selector '[Id SHCatalog] (Id SHSession)
initWithCatalogSelector = mkSelector "initWithCatalog:"

-- | @Selector@ for @matchStreamingBuffer:atTime:@
matchStreamingBuffer_atTimeSelector :: Selector '[Id AVAudioPCMBuffer, Id AVAudioTime] ()
matchStreamingBuffer_atTimeSelector = mkSelector "matchStreamingBuffer:atTime:"

-- | @Selector@ for @matchSignature:@
matchSignatureSelector :: Selector '[Id SHSignature] ()
matchSignatureSelector = mkSelector "matchSignature:"

-- | @Selector@ for @catalog@
catalogSelector :: Selector '[] (Id SHCatalog)
catalogSelector = mkSelector "catalog"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

