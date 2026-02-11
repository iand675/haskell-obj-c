{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedString@.
module ObjC.WebKit.NSAttributedString
  ( NSAttributedString
  , IsNSAttributedString(..)
  , loadFromHTMLWithRequest_options_completionHandler
  , loadFromHTMLWithFileURL_options_completionHandler
  , loadFromHTMLWithString_options_completionHandler
  , loadFromHTMLWithData_options_completionHandler
  , loadFromHTMLWithRequest_options_completionHandlerSelector
  , loadFromHTMLWithFileURL_options_completionHandlerSelector
  , loadFromHTMLWithString_options_completionHandlerSelector
  , loadFromHTMLWithData_options_completionHandlerSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Loads an HTML URL request and converts the contents into an attributed string.
--
-- @request@ — The request specifying the URL to load.
--
-- @options@ — Document attributes for interpreting the document contents. NSTextSizeMultiplierDocumentOption and NSTimeoutDocumentOption are supported option keys.
--
-- @completionHandler@ — A block to invoke when the operation completes or fails.
--
-- The completionHandler is passed the attributed string result along with any document-level attributes, or an error.
--
-- ObjC selector: @+ loadFromHTMLWithRequest:options:completionHandler:@
loadFromHTMLWithRequest_options_completionHandler :: (IsNSURLRequest request, IsNSDictionary options) => request -> options -> Ptr () -> IO ()
loadFromHTMLWithRequest_options_completionHandler request options completionHandler =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr request $ \raw_request ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "loadFromHTMLWithRequest:options:completionHandler:") retVoid [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Converts a local HTML file into an attributed string.
--
-- @fileURL@ — The file URL to load.
--
-- @options@ — Document attributes for interpreting the document contents. NSTextSizeMultiplierDocumentOption, NSTimeoutDocumentOption and NSReadAccessURLDocumentOption are supported option keys.
--
-- @completionHandler@ — A block to invoke when the operation completes or fails.
--
-- The completionHandler is passed the attributed string result along with any document-level attributes, or an error. If NSReadAccessURLDocumentOption references a single file, only that file may be loaded by WebKit. If NSReadAccessURLDocumentOption references a directory, files inside that directory may be loaded by WebKit.
--
-- ObjC selector: @+ loadFromHTMLWithFileURL:options:completionHandler:@
loadFromHTMLWithFileURL_options_completionHandler :: (IsNSURL fileURL, IsNSDictionary options) => fileURL -> options -> Ptr () -> IO ()
loadFromHTMLWithFileURL_options_completionHandler fileURL options completionHandler =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr fileURL $ \raw_fileURL ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "loadFromHTMLWithFileURL:options:completionHandler:") retVoid [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Converts an HTML string into an attributed string.
--
-- @string@ — The HTML string to use as the contents.
--
-- @options@ — Document attributes for interpreting the document contents. NSTextSizeMultiplierDocumentOption, NSTimeoutDocumentOption and NSBaseURLDocumentOption are supported option keys.
--
-- @completionHandler@ — A block to invoke when the operation completes or fails.
--
-- The completionHandler is passed the attributed string result along with any document-level attributes, or an error. NSBaseURLDocumentOption is used to resolve relative URLs within the document.
--
-- ObjC selector: @+ loadFromHTMLWithString:options:completionHandler:@
loadFromHTMLWithString_options_completionHandler :: (IsNSString string, IsNSDictionary options) => string -> options -> Ptr () -> IO ()
loadFromHTMLWithString_options_completionHandler string options completionHandler =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr string $ \raw_string ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "loadFromHTMLWithString:options:completionHandler:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Converts HTML data into an attributed string.
--
-- @data@ — The HTML data to use as the contents.
--
-- @options@ — Document attributes for interpreting the document contents. NSTextSizeMultiplierDocumentOption, NSTimeoutDocumentOption, NSTextEncodingNameDocumentOption, and NSCharacterEncodingDocumentOption are supported option keys.
--
-- @completionHandler@ — A block to invoke when the operation completes or fails.
--
-- The completionHandler is passed the attributed string result along with any document-level attributes, or an error. If neither NSTextEncodingNameDocumentOption nor NSCharacterEncodingDocumentOption is supplied, a best-guess encoding is used.
--
-- ObjC selector: @+ loadFromHTMLWithData:options:completionHandler:@
loadFromHTMLWithData_options_completionHandler :: (IsNSData data_, IsNSDictionary options) => data_ -> options -> Ptr () -> IO ()
loadFromHTMLWithData_options_completionHandler data_ options completionHandler =
  do
    cls' <- getRequiredClass "NSAttributedString"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "loadFromHTMLWithData:options:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromHTMLWithRequest:options:completionHandler:@
loadFromHTMLWithRequest_options_completionHandlerSelector :: Selector
loadFromHTMLWithRequest_options_completionHandlerSelector = mkSelector "loadFromHTMLWithRequest:options:completionHandler:"

-- | @Selector@ for @loadFromHTMLWithFileURL:options:completionHandler:@
loadFromHTMLWithFileURL_options_completionHandlerSelector :: Selector
loadFromHTMLWithFileURL_options_completionHandlerSelector = mkSelector "loadFromHTMLWithFileURL:options:completionHandler:"

-- | @Selector@ for @loadFromHTMLWithString:options:completionHandler:@
loadFromHTMLWithString_options_completionHandlerSelector :: Selector
loadFromHTMLWithString_options_completionHandlerSelector = mkSelector "loadFromHTMLWithString:options:completionHandler:"

-- | @Selector@ for @loadFromHTMLWithData:options:completionHandler:@
loadFromHTMLWithData_options_completionHandlerSelector :: Selector
loadFromHTMLWithData_options_completionHandlerSelector = mkSelector "loadFromHTMLWithData:options:completionHandler:"

