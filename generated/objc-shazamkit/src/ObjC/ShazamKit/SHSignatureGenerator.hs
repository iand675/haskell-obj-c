{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object for converting audio data into a signature.
--
-- Create both reference and query signatures using this class.
--
-- Generated bindings for @SHSignatureGenerator@.
module ObjC.ShazamKit.SHSignatureGenerator
  ( SHSignatureGenerator
  , IsSHSignatureGenerator(..)
  , generateSignatureFromAsset_completionHandler
  , appendBuffer_atTime_error
  , signature
  , generateSignatureFromAsset_completionHandlerSelector
  , appendBuffer_atTime_errorSelector
  , signatureSelector


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
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a signature with the asset you specify.
--
-- > Important: > You can call this method from synchronous code using a completion handler, as shown on this page, or you can call it as an asynchronous method that has the following declaration: > > ```swift > class func signature(from asset: AVAsset) async throws -> SHSignature > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- The asset you specify can be any type of media that contains audio tracks. If the asset has multiple tracks, the system mixes them into one ``SHSignature``.
--
-- - Parameters:   - asset: An asset that contains the audio to convert.   - completionHandler: The system calls this completion block after creating the signature, or an error if the system couldn't create it.
--
-- This block takes the following parameters:
--
-- - term @signature@: A new signature instance.     - term error: An error object if a problem occurs when creating thesignature; otherwise, @nil@.
--
-- ObjC selector: @+ generateSignatureFromAsset:completionHandler:@
generateSignatureFromAsset_completionHandler :: IsAVAsset asset => asset -> Ptr () -> IO ()
generateSignatureFromAsset_completionHandler asset completionHandler =
  do
    cls' <- getRequiredClass "SHSignatureGenerator"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "generateSignatureFromAsset:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Adds audio to the generator.
--
-- Using noncontiguous audio may result in a lower-quality signature.
--
-- The audio must be PCM at one of these rates:
--
-- - @48000@ hertz - @44100@ hertz - @32000@ hertz - @16000@ hertz
--
-- - Parameters:   - buffer: The audio data to append to the signature generator.   - time: The time position of the start of the audio buffer in the full audio you use to generate the signature.   - error: The error that occurs; otherwise, @nil@.
--
-- ObjC selector: @- appendBuffer:atTime:error:@
appendBuffer_atTime_error :: (IsSHSignatureGenerator shSignatureGenerator, IsAVAudioPCMBuffer buffer, IsAVAudioTime time, IsNSError error_) => shSignatureGenerator -> buffer -> time -> error_ -> IO Bool
appendBuffer_atTime_error shSignatureGenerator  buffer time error_ =
withObjCPtr buffer $ \raw_buffer ->
  withObjCPtr time $ \raw_time ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg shSignatureGenerator (mkSelector "appendBuffer:atTime:error:") retCULong [argPtr (castPtr raw_buffer :: Ptr ()), argPtr (castPtr raw_time :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Converts the audio buffer into a signature.
--
-- - Returns: A signature that ShazamKit generates from the audio buffer.
--
-- ObjC selector: @- signature@
signature :: IsSHSignatureGenerator shSignatureGenerator => shSignatureGenerator -> IO (Id SHSignature)
signature shSignatureGenerator  =
  sendMsg shSignatureGenerator (mkSelector "signature") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateSignatureFromAsset:completionHandler:@
generateSignatureFromAsset_completionHandlerSelector :: Selector
generateSignatureFromAsset_completionHandlerSelector = mkSelector "generateSignatureFromAsset:completionHandler:"

-- | @Selector@ for @appendBuffer:atTime:error:@
appendBuffer_atTime_errorSelector :: Selector
appendBuffer_atTime_errorSelector = mkSelector "appendBuffer:atTime:error:"

-- | @Selector@ for @signature@
signatureSelector :: Selector
signatureSelector = mkSelector "signature"

