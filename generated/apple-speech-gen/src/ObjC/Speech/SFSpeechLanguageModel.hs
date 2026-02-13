{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A language model built from custom training data.
--
-- Create this object using ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:completion:`` or ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:ignoresCache:completion:``.
--
-- Generated bindings for @SFSpeechLanguageModel@.
module ObjC.Speech.SFSpeechLanguageModel
  ( SFSpeechLanguageModel
  , IsSFSpeechLanguageModel(..)
  , prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completion
  , prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completion
  , prepareCustomLanguageModelForUrl_configuration_completion
  , prepareCustomLanguageModelForUrl_configuration_ignoresCache_completion
  , prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completionSelector
  , prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completionSelector
  , prepareCustomLanguageModelForUrl_configuration_completionSelector
  , prepareCustomLanguageModelForUrl_configuration_ignoresCache_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ prepareCustomLanguageModelForUrl:clientIdentifier:configuration:completion:@
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completion :: (IsNSURL asset, IsNSString clientIdentifier, IsSFSpeechLanguageModelConfiguration configuration) => asset -> clientIdentifier -> configuration -> Ptr () -> IO ()
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completion asset clientIdentifier configuration completion =
  do
    cls' <- getRequiredClass "SFSpeechLanguageModel"
    sendClassMessage cls' prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completionSelector (toNSURL asset) (toNSString clientIdentifier) (toSFSpeechLanguageModelConfiguration configuration) completion

-- | @+ prepareCustomLanguageModelForUrl:clientIdentifier:configuration:ignoresCache:completion:@
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completion :: (IsNSURL asset, IsNSString clientIdentifier, IsSFSpeechLanguageModelConfiguration configuration) => asset -> clientIdentifier -> configuration -> Bool -> Ptr () -> IO ()
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completion asset clientIdentifier configuration ignoresCache completion =
  do
    cls' <- getRequiredClass "SFSpeechLanguageModel"
    sendClassMessage cls' prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completionSelector (toNSURL asset) (toNSString clientIdentifier) (toSFSpeechLanguageModelConfiguration configuration) ignoresCache completion

-- | Creates a language model from custom training data.
--
-- - Parameters:    - asset: The URL of a file containing custom training data. Create this file with ``SFCustomLanguageModelData/export(to:)``.    - configuration: An object listing the URLs at which this method should create the language model and compiled vocabulary from the training data.    - completion: Called when the language model has been created.
--
-- ObjC selector: @+ prepareCustomLanguageModelForUrl:configuration:completion:@
prepareCustomLanguageModelForUrl_configuration_completion :: (IsNSURL asset, IsSFSpeechLanguageModelConfiguration configuration) => asset -> configuration -> Ptr () -> IO ()
prepareCustomLanguageModelForUrl_configuration_completion asset configuration completion =
  do
    cls' <- getRequiredClass "SFSpeechLanguageModel"
    sendClassMessage cls' prepareCustomLanguageModelForUrl_configuration_completionSelector (toNSURL asset) (toSFSpeechLanguageModelConfiguration configuration) completion

-- | Creates a language model from custom training data.
--
-- - Parameters:    - asset: The URL of a file containing custom training data. Create this file with ``SFCustomLanguageModelData/export(to:)``.    - configuration: An object listing the URLs at which this method should create the language model and compiled vocabulary from the training data.    - ignoresCache: If @true@, the language model identified by the configuration will be recreated even if the @asset@ file is unchanged.    - completion: Called when the language model has been created.
--
-- ObjC selector: @+ prepareCustomLanguageModelForUrl:configuration:ignoresCache:completion:@
prepareCustomLanguageModelForUrl_configuration_ignoresCache_completion :: (IsNSURL asset, IsSFSpeechLanguageModelConfiguration configuration) => asset -> configuration -> Bool -> Ptr () -> IO ()
prepareCustomLanguageModelForUrl_configuration_ignoresCache_completion asset configuration ignoresCache completion =
  do
    cls' <- getRequiredClass "SFSpeechLanguageModel"
    sendClassMessage cls' prepareCustomLanguageModelForUrl_configuration_ignoresCache_completionSelector (toNSURL asset) (toSFSpeechLanguageModelConfiguration configuration) ignoresCache completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareCustomLanguageModelForUrl:clientIdentifier:configuration:completion:@
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completionSelector :: Selector '[Id NSURL, Id NSString, Id SFSpeechLanguageModelConfiguration, Ptr ()] ()
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_completionSelector = mkSelector "prepareCustomLanguageModelForUrl:clientIdentifier:configuration:completion:"

-- | @Selector@ for @prepareCustomLanguageModelForUrl:clientIdentifier:configuration:ignoresCache:completion:@
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completionSelector :: Selector '[Id NSURL, Id NSString, Id SFSpeechLanguageModelConfiguration, Bool, Ptr ()] ()
prepareCustomLanguageModelForUrl_clientIdentifier_configuration_ignoresCache_completionSelector = mkSelector "prepareCustomLanguageModelForUrl:clientIdentifier:configuration:ignoresCache:completion:"

-- | @Selector@ for @prepareCustomLanguageModelForUrl:configuration:completion:@
prepareCustomLanguageModelForUrl_configuration_completionSelector :: Selector '[Id NSURL, Id SFSpeechLanguageModelConfiguration, Ptr ()] ()
prepareCustomLanguageModelForUrl_configuration_completionSelector = mkSelector "prepareCustomLanguageModelForUrl:configuration:completion:"

-- | @Selector@ for @prepareCustomLanguageModelForUrl:configuration:ignoresCache:completion:@
prepareCustomLanguageModelForUrl_configuration_ignoresCache_completionSelector :: Selector '[Id NSURL, Id SFSpeechLanguageModelConfiguration, Bool, Ptr ()] ()
prepareCustomLanguageModelForUrl_configuration_ignoresCache_completionSelector = mkSelector "prepareCustomLanguageModelForUrl:configuration:ignoresCache:completion:"

