{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKTextureLoader
--
-- Load Metal textures from files with the device specified at initialization
--
-- Generated bindings for @MTKTextureLoader@.
module ObjC.MetalKit.MTKTextureLoader
  ( MTKTextureLoader
  , IsMTKTextureLoader(..)
  , init_
  , initWithDevice
  , newTextureWithContentsOfURL_options_completionHandler
  , newTextureWithName_scaleFactor_bundle_options_completionHandler
  , newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandler
  , newTexturesWithContentsOfURLs_options_completionHandler
  , newTexturesWithNames_scaleFactor_bundle_options_completionHandler
  , newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandler
  , newTextureWithData_options_completionHandler
  , newTextureWithCGImage_options_completionHandler
  , newTextureWithMDLTexture_options_completionHandler
  , newTextureWithContentsOfURL_options_error
  , newTexturesWithContentsOfURLs_options_error
  , newTextureWithData_options_error
  , newTextureWithCGImage_options_error
  , newTextureWithMDLTexture_options_error
  , newTextureWithName_scaleFactor_bundle_options_error
  , newTextureWithName_scaleFactor_displayGamut_bundle_options_error
  , device
  , deviceSelector
  , initSelector
  , initWithDeviceSelector
  , newTextureWithCGImage_options_completionHandlerSelector
  , newTextureWithCGImage_options_errorSelector
  , newTextureWithContentsOfURL_options_completionHandlerSelector
  , newTextureWithContentsOfURL_options_errorSelector
  , newTextureWithData_options_completionHandlerSelector
  , newTextureWithData_options_errorSelector
  , newTextureWithMDLTexture_options_completionHandlerSelector
  , newTextureWithMDLTexture_options_errorSelector
  , newTextureWithName_scaleFactor_bundle_options_completionHandlerSelector
  , newTextureWithName_scaleFactor_bundle_options_errorSelector
  , newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandlerSelector
  , newTextureWithName_scaleFactor_displayGamut_bundle_options_errorSelector
  , newTexturesWithContentsOfURLs_options_completionHandlerSelector
  , newTexturesWithContentsOfURLs_options_errorSelector
  , newTexturesWithNames_scaleFactor_bundle_options_completionHandlerSelector
  , newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandlerSelector

  -- * Enum types
  , NSDisplayGamut(NSDisplayGamut)
  , pattern NSDisplayGamutSRGB
  , pattern NSDisplayGamutP3

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.ModelIO.Internal.Classes

-- | @- init@
init_ :: IsMTKTextureLoader mtkTextureLoader => mtkTextureLoader -> IO (Id MTKTextureLoader)
init_ mtkTextureLoader =
  sendOwnedMessage mtkTextureLoader initSelector

-- | initWithDevice:
--
-- Initialize the loader
--
-- @device@ — Metal device with which to create Metal textures
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMTKTextureLoader mtkTextureLoader => mtkTextureLoader -> RawId -> IO (Id MTKTextureLoader)
initWithDevice mtkTextureLoader device =
  sendOwnedMessage mtkTextureLoader initWithDeviceSelector device

-- | newTextureWithContentsOfURL:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from the file at URL
--
-- @URL@ — Location of image file from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @completionHandler@ — Block called when the texture has been loaded and fully initialized
--
-- ObjC selector: @- newTextureWithContentsOfURL:options:completionHandler:@
newTextureWithContentsOfURL_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSURL url, IsNSDictionary options) => mtkTextureLoader -> url -> options -> Ptr () -> IO ()
newTextureWithContentsOfURL_options_completionHandler mtkTextureLoader url options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithContentsOfURL_options_completionHandlerSelector (toNSURL url) (toNSDictionary options) completionHandler

-- | newTextureWithName:scaleFactor:bundle:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from a given texture or image            asset name
--
-- @name@ — A texture or image asset name
--
-- @scaleFactor@ — scale factor of the texture to retrieve from the asset catalog.  Typically the                     value retrieved from -[UIView contentScale] or -[NSWindow backingScaleFactor].
--
-- @bundle@ — Resource bundle in which the asset is located.  Main bundle used if nil.
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used                to load a texture asset but can be used when creating a texture from an image asset:                    MTKTextureLoaderOptionGenerateMipmaps                    MTKTextureLoaderOptionSRGB                    MTKTextureLoaderOptionCubeFromVerticalTexture                    MTKTextureLoaderOptionOrigin
--
-- @completionHandler@ — Block called when texture has been loaded and fully initialized
--
-- Uses texture data from version of the texture from the texture set in the asset catalog             which mathces the device's traits.             This method attempts to load a texture asset with thw name iven.  If a texture asset             with the name given does not exist, it will attempt to create a texture from an             image asset with the given name
--
-- ObjC selector: @- newTextureWithName:scaleFactor:bundle:options:completionHandler:@
newTextureWithName_scaleFactor_bundle_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options) => mtkTextureLoader -> name -> CDouble -> bundle -> options -> Ptr () -> IO ()
newTextureWithName_scaleFactor_bundle_options_completionHandler mtkTextureLoader name scaleFactor bundle options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithName_scaleFactor_bundle_options_completionHandlerSelector (toNSString name) scaleFactor (toNSBundle bundle) (toNSDictionary options) completionHandler

-- | newTextureWithName:scaleFactor:displayGamut:bundle:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from a given texture or image            asset name
--
-- @name@ — A texture or image asset name
--
-- @scaleFactor@ — Scale factor of the texture to retrieve from the asset catalog.  Typically the                    value retrieved from -[NSWindow backingScaleFactor].
--
-- @displayGamut@ — Version of the texture based upon the "Gamut" trait in Xcode.  You'd typically                     check -[NSWindow canRepresentDisplayGamut:] with the widest NSDisplayGamut value                     and pass that value here if it returns YES.
--
-- @bundle@ — Resource bundle in which the asset is located.  Main bundle used if nil.
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used                to load a texture asset but can be used when creating a texture from an image asset:                    MTKTextureLoaderOptionGenerateMipmaps                    MTKTextureLoaderOptionSRGB                    MTKTextureLoaderOptionCubeFromVerticalTexture                    MTKTextureLoaderOptionOrigin
--
-- @completionHandler@ — Block called when texture has been loaded and fully initialized
--
-- Uses texture data from version of the texture from the texture set in the asset catalog             which mathces the device's traits.             This method attempts to load a texture asset with the name given.  If a texture asset             with the name given does not exist, it will attempt to create a texture from an             image asset with the given name.             This method can be used on macOS to choose between sRGB and P3 versions of a texture             asset depending on the gamut of the display rendered to.
--
-- ObjC selector: @- newTextureWithName:scaleFactor:displayGamut:bundle:options:completionHandler:@
newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options) => mtkTextureLoader -> name -> CDouble -> NSDisplayGamut -> bundle -> options -> Ptr () -> IO ()
newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandler mtkTextureLoader name scaleFactor displayGamut bundle options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandlerSelector (toNSString name) scaleFactor displayGamut (toNSBundle bundle) (toNSDictionary options) completionHandler

-- | newTexturesWithContentsOfURLs:options:completionHandler:
--
-- Asynchronously create an array of Metal textures and load image data from the files at URLs
--
-- @URLs@ — Locations of image files from which to create the textures
--
-- @options@ — Dictonary of MTKTextureLoaderOptions, which will be used for every texture loaded
--
-- @completionHandler@ — Block called when all of the textures have been loaded and fully initialized. The array of MTLTextures will be the same length and in the same order as the requested array of paths. If an error occurs while loading a texture, the corresponding array index will contain NSNull. The NSError will be null if all of the textures are loaded successfully, or will correspond to one of the textures which failed to load.
--
-- ObjC selector: @- newTexturesWithContentsOfURLs:options:completionHandler:@
newTexturesWithContentsOfURLs_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSArray urLs, IsNSDictionary options) => mtkTextureLoader -> urLs -> options -> Ptr () -> IO ()
newTexturesWithContentsOfURLs_options_completionHandler mtkTextureLoader urLs options completionHandler =
  sendOwnedMessage mtkTextureLoader newTexturesWithContentsOfURLs_options_completionHandlerSelector (toNSArray urLs) (toNSDictionary options) completionHandler

-- | newTexturesWithNames:scaleFactor:bundle:options:completionHandler:
--
-- Asynchronously create Metal textures and load image data from a given texture or image           asset names
--
-- @names@ — An array texture or image asset names.  If an error occurs while loading a texture,              the corresponding index in the returned array contain [NSNull null]
--
-- @scaleFactor@ — scale factor of the texture to retrieve from the asset catalog.  Typically the                    value retrieved from -[UIView contentScale] or -[NSWindow backingScaleFactor].
--
-- @bundle@ — Resource bundle in which the assets are located.  Main bundle used if nil.
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used                to load a texture asset but can be used when creating a texture from an image asset                    MTKTextureLoaderOptionGenerateMipmaps                    MTKTextureLoaderOptionSRGB                    MTKTextureLoaderOptionCubeFromVerticalTexture                    MTKTextureLoaderOptionOrigin
--
-- @completionHandler@ — Block called when all of the textures have been loaded and fully                          initialized. The NSError will be null if all of the textures are loaded                          successfully, or will correspond to one of the textures which failed to                          load.
--
-- Uses texture data from version of the texture from the texture set in the asset catalog             which mathces the device's traits.             This method attempts to load a texture asset with each name iven.  If a texture asset             with the name given does not exist, it will attempt to create a texture from an             image asset with the given name.
--
-- ObjC selector: @- newTexturesWithNames:scaleFactor:bundle:options:completionHandler:@
newTexturesWithNames_scaleFactor_bundle_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSArray names, IsNSBundle bundle, IsNSDictionary options) => mtkTextureLoader -> names -> CDouble -> bundle -> options -> Ptr () -> IO ()
newTexturesWithNames_scaleFactor_bundle_options_completionHandler mtkTextureLoader names scaleFactor bundle options completionHandler =
  sendOwnedMessage mtkTextureLoader newTexturesWithNames_scaleFactor_bundle_options_completionHandlerSelector (toNSArray names) scaleFactor (toNSBundle bundle) (toNSDictionary options) completionHandler

-- | newTexturesWithNames:scaleFactor:displayGamut:bundle:options:completionHandler:
--
-- Asynchronously create Metal textures and load image data from given texture or image          asset names
--
-- @names@ — An array texture or image asset names.  If an error occurs while loading a texture,             the corresponding index in the returned array contain [NSNull null]
--
-- @scaleFactor@ — Scale factor of the texture to retrieve from the asset catalog.  Typically the                   value retrieved from -[UIView contentScale] or -[NSWindow backingScaleFactor]
--
-- @displayGamut@ — Version of the texture based upon the "Gamut" trait in Xcode.  You'd typically                    check -[NSWindow canRepresentDisplayGamut:] with the widest NSDisplayGamut value                    and pass that value here if it returns YES.
--
-- @bundle@ — Resource bundle in which the assets are located
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used               to load a texture asset but can be used when creating a texture from an image asset                   MTKTextureLoaderOptionGenerateMipmaps                   MTKTextureLoaderOptionSRGB                   MTKTextureLoaderOptionCubeFromVerticalTexture                   MTKTextureLoaderOptionOrigin
--
-- @completionHandler@ — Block called when all of the textures have been loaded and fully                         initialized. The NSError will be nif if all of the textures are loaded                         successfully, or will correspond to one of the textures which failed to                         load.
--
-- Uses texture data from version of the texture from the texture sets in the asset catalog            which mathces the device's traits.            This method attempts to load a texture asset with each name given.  If a texture asset            with the name given does not exist, it will attempt to create a texture from an            image asset with the given name.            This method can be used on macOS to choose between sRGB and P3 versions of a texture            asset depending on the gamut of the display rendered to,            If a texture with a name fails to load, the correposding index in the returned array            will be set to [NSNull null].  An error will also be set.  Thus, if there is a failure            to load a texture with a name, other names may succesfully be loaded.  Also, a set            error does not necessarily mean all textures in the names array have failed to load.
--
-- ObjC selector: @- newTexturesWithNames:scaleFactor:displayGamut:bundle:options:completionHandler:@
newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSArray names, IsNSBundle bundle, IsNSDictionary options) => mtkTextureLoader -> names -> CDouble -> NSDisplayGamut -> bundle -> options -> Ptr () -> IO ()
newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandler mtkTextureLoader names scaleFactor displayGamut bundle options completionHandler =
  sendOwnedMessage mtkTextureLoader newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandlerSelector (toNSArray names) scaleFactor displayGamut (toNSBundle bundle) (toNSDictionary options) completionHandler

-- | newTextureWithData:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from the NSData object provided
--
-- @data@ — NSData object containing image file data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @completionHandler@ — Block called when texture has been loaded and fully initialized
--
-- ObjC selector: @- newTextureWithData:options:completionHandler:@
newTextureWithData_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSData data_, IsNSDictionary options) => mtkTextureLoader -> data_ -> options -> Ptr () -> IO ()
newTextureWithData_options_completionHandler mtkTextureLoader data_ options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithData_options_completionHandlerSelector (toNSData data_) (toNSDictionary options) completionHandler

-- | newTextureWithCGImage:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from the given CGImageRef
--
-- @cgImage@ — CGImageRef containing image data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @completionHandler@ — Block called when texture has been loaded and fully initialized
--
-- ObjC selector: @- newTextureWithCGImage:options:completionHandler:@
newTextureWithCGImage_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsNSDictionary options) => mtkTextureLoader -> Ptr () -> options -> Ptr () -> IO ()
newTextureWithCGImage_options_completionHandler mtkTextureLoader cgImage options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithCGImage_options_completionHandlerSelector cgImage (toNSDictionary options) completionHandler

-- | newTextureWithMDLTexture:options:completionHandler:
--
-- Asynchronously create a Metal texture and load image data from the given MDLTexture
--
-- @texture@ — MDLTexture containing image data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @completionHandler@ — Block called when texture has been loaded and fully initialized
--
-- ObjC selector: @- newTextureWithMDLTexture:options:completionHandler:@
newTextureWithMDLTexture_options_completionHandler :: (IsMTKTextureLoader mtkTextureLoader, IsMDLTexture texture, IsNSDictionary options) => mtkTextureLoader -> texture -> options -> Ptr () -> IO ()
newTextureWithMDLTexture_options_completionHandler mtkTextureLoader texture options completionHandler =
  sendOwnedMessage mtkTextureLoader newTextureWithMDLTexture_options_completionHandlerSelector (toMDLTexture texture) (toNSDictionary options) completionHandler

-- | newTextureWithContentsOfURL:options:error:
--
-- Synchronously create a Metal texture and load image data from the file at URL
--
-- Returns: The Metal texture. nil if an error occured
--
-- @URL@ — Location of image file from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @error@ — Pointer to an autoreleased NSError object which will be set if an error occurred
--
-- ObjC selector: @- newTextureWithContentsOfURL:options:error:@
newTextureWithContentsOfURL_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSURL url, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> url -> options -> error_ -> IO RawId
newTextureWithContentsOfURL_options_error mtkTextureLoader url options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithContentsOfURL_options_errorSelector (toNSURL url) (toNSDictionary options) (toNSError error_)

-- | newTexturesWithContentsOfURLs:options:completionHandler:
--
-- Synchronously create an array of Metal textures and load image data from the files at URLs
--
-- Returns: An array of MTLTextures of the same length and in the same order as the requested array of         paths.  If an error occurs while loading a texture, the corresponding array index will         contain [NSNull null].
--
-- @URLs@ — Locations of image files from which to create the textures
--
-- @options@ — Dictonary of MTKTextureLoaderOptions, which will be used for every texture loaded
--
-- @error@ — Pointer to an autoreleased NSError object which will be set if an error occurred.              Will be null if all of the textures are loaded successfully, or will correspond to              one of the textures which failed to load.
--
-- ObjC selector: @- newTexturesWithContentsOfURLs:options:error:@
newTexturesWithContentsOfURLs_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSArray urLs, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> urLs -> options -> error_ -> IO (Id NSArray)
newTexturesWithContentsOfURLs_options_error mtkTextureLoader urLs options error_ =
  sendOwnedMessage mtkTextureLoader newTexturesWithContentsOfURLs_options_errorSelector (toNSArray urLs) (toNSDictionary options) (toNSError error_)

-- | newTextureWithData:options:error:
--
-- Synchronously create a Metal texture and load image data from the NSData object provided
--
-- Returns: The Metal texture. nil if an error occured
--
-- @data@ — NSData object containing image file data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @error@ — Pointer to an autoreleased NSError object which will be set if an error occurred
--
-- ObjC selector: @- newTextureWithData:options:error:@
newTextureWithData_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSData data_, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> data_ -> options -> error_ -> IO RawId
newTextureWithData_options_error mtkTextureLoader data_ options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithData_options_errorSelector (toNSData data_) (toNSDictionary options) (toNSError error_)

-- | newTextureWithCGImage:options:error:
--
-- Synchronously create a Metal texture and load image data from the given CGImageRef
--
-- Returns: The Metal texture. nil if an error occured
--
-- @cgImage@ — CGImageRef containing image data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @error@ — Pointer to an autoreleased NSError object which will be set if an error occurred
--
-- ObjC selector: @- newTextureWithCGImage:options:error:@
newTextureWithCGImage_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> Ptr () -> options -> error_ -> IO RawId
newTextureWithCGImage_options_error mtkTextureLoader cgImage options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithCGImage_options_errorSelector cgImage (toNSDictionary options) (toNSError error_)

-- | newTextureWithMDLTexture:options:error:
--
-- Synchronously create a Metal texture and load image data from the given MDLTexture
--
-- Returns: The Metal texture. nil if an error occured
--
-- @texture@ — MDLTexture containing image data from which to create the texture
--
-- @options@ — Dictonary of MTKTextureLoaderOptions
--
-- @error@ — Pointer to an autoreleased NSError object which will be set if an error occurred
--
-- ObjC selector: @- newTextureWithMDLTexture:options:error:@
newTextureWithMDLTexture_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsMDLTexture texture, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> texture -> options -> error_ -> IO RawId
newTextureWithMDLTexture_options_error mtkTextureLoader texture options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithMDLTexture_options_errorSelector (toMDLTexture texture) (toNSDictionary options) (toNSError error_)

-- | newTextursWithName:scaleFactor:bundle:options:error:
--
-- Synchronously create a Metal texture with texture data from a given texture or image            asset name
--
-- Returns: The Metal texture. nil if an error occured
--
-- @names@ — An array of texture asset names
--
-- @scaleFactor@ — scale factor of the texture to retrieve from the asset catalog.  Typically the                    value retrieved from -[UIView contentScale] or -[NSWindow backingScaleFactor].
--
-- @bundle@ — Resource bundle in which the asset is located.  Main bundle used if nil.
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used                to load a texture asset but can be used when creating a texture from an image asset                    MTKTextureLoaderOptionGenerateMipmaps                    MTKTextureLoaderOptionSRGB                    MTKTextureLoaderOptionCubeFromVerticalTexture                    MTKTextureLoaderOptionOrigins
--
-- Uses texture data from version of the texture from the texture set in the asset catalog             which mathces the device's traits.             This method attempts to load a texture asset with the name given.  If a texture asset             with the name given does not exist, it will attempt to create a texture from an             image asset with the given name.
--
-- ObjC selector: @- newTextureWithName:scaleFactor:bundle:options:error:@
newTextureWithName_scaleFactor_bundle_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> name -> CDouble -> bundle -> options -> error_ -> IO RawId
newTextureWithName_scaleFactor_bundle_options_error mtkTextureLoader name scaleFactor bundle options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithName_scaleFactor_bundle_options_errorSelector (toNSString name) scaleFactor (toNSBundle bundle) (toNSDictionary options) (toNSError error_)

-- | newTextursWithName:scaleFactor:displayGamut:bundle:options:error:
--
-- Synchronously create a Metal texture with texture data from a given texture or image           asset name
--
-- Returns: The Metal texture. nil if an error occured
--
-- @names@ — An array of texture asset names
--
-- @scaleFactor@ — Scale factor of the texture to retrieve from the asset catalog.  Typically the                    value retrieved from -[UIView contentScale] or -[NSWindow backingScaleFactor].
--
-- @displayGamut@ — Version of the texture based upon the "Gamut" trait in Xcode.  You'd typically                     check -[NSWindow canRepresentDisplayGamut:] with the widest NSDisplayGamut value                     and pass that value here if it returns YES.
--
-- @bundle@ — Resource bundle in which the assets are located
--
-- @bundle@ — Resource bundle in which the asset is located.  Main bundle used if nil.
--
-- @options@ — Dictonary of MTKTextureLoaderOptions. The following options are ignormed when used                to load a texture asset but can be used when creating a texture from an image asset                    MTKTextureLoaderOptionGenerateMipmaps                    MTKTextureLoaderOptionSRGB                    MTKTextureLoaderOptionCubeFromVerticalTexture                    MTKTextureLoaderOptionOrigin
--
-- Uses texture data from version of the texture from the texture set in the asset catalog             which mathces the device's traits.             This method attempts to load a texture asset with the name given.  If a texture asset             with the name given does not exist, it will attempt to create a texture from an             image asset with the given name.             This method can be used on macOS to choose between sRGB and P3 versions of a texture             asset depending on the gamut of the display rendered to.
--
-- ObjC selector: @- newTextureWithName:scaleFactor:displayGamut:bundle:options:error:@
newTextureWithName_scaleFactor_displayGamut_bundle_options_error :: (IsMTKTextureLoader mtkTextureLoader, IsNSString name, IsNSBundle bundle, IsNSDictionary options, IsNSError error_) => mtkTextureLoader -> name -> CDouble -> NSDisplayGamut -> bundle -> options -> error_ -> IO RawId
newTextureWithName_scaleFactor_displayGamut_bundle_options_error mtkTextureLoader name scaleFactor displayGamut bundle options error_ =
  sendOwnedMessage mtkTextureLoader newTextureWithName_scaleFactor_displayGamut_bundle_options_errorSelector (toNSString name) scaleFactor displayGamut (toNSBundle bundle) (toNSDictionary options) (toNSError error_)

-- | device
--
-- Metal device with which to create Metal textures
--
-- ObjC selector: @- device@
device :: IsMTKTextureLoader mtkTextureLoader => mtkTextureLoader -> IO RawId
device mtkTextureLoader =
  sendMessage mtkTextureLoader deviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTKTextureLoader)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MTKTextureLoader)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @newTextureWithContentsOfURL:options:completionHandler:@
newTextureWithContentsOfURL_options_completionHandlerSelector :: Selector '[Id NSURL, Id NSDictionary, Ptr ()] ()
newTextureWithContentsOfURL_options_completionHandlerSelector = mkSelector "newTextureWithContentsOfURL:options:completionHandler:"

-- | @Selector@ for @newTextureWithName:scaleFactor:bundle:options:completionHandler:@
newTextureWithName_scaleFactor_bundle_options_completionHandlerSelector :: Selector '[Id NSString, CDouble, Id NSBundle, Id NSDictionary, Ptr ()] ()
newTextureWithName_scaleFactor_bundle_options_completionHandlerSelector = mkSelector "newTextureWithName:scaleFactor:bundle:options:completionHandler:"

-- | @Selector@ for @newTextureWithName:scaleFactor:displayGamut:bundle:options:completionHandler:@
newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandlerSelector :: Selector '[Id NSString, CDouble, NSDisplayGamut, Id NSBundle, Id NSDictionary, Ptr ()] ()
newTextureWithName_scaleFactor_displayGamut_bundle_options_completionHandlerSelector = mkSelector "newTextureWithName:scaleFactor:displayGamut:bundle:options:completionHandler:"

-- | @Selector@ for @newTexturesWithContentsOfURLs:options:completionHandler:@
newTexturesWithContentsOfURLs_options_completionHandlerSelector :: Selector '[Id NSArray, Id NSDictionary, Ptr ()] ()
newTexturesWithContentsOfURLs_options_completionHandlerSelector = mkSelector "newTexturesWithContentsOfURLs:options:completionHandler:"

-- | @Selector@ for @newTexturesWithNames:scaleFactor:bundle:options:completionHandler:@
newTexturesWithNames_scaleFactor_bundle_options_completionHandlerSelector :: Selector '[Id NSArray, CDouble, Id NSBundle, Id NSDictionary, Ptr ()] ()
newTexturesWithNames_scaleFactor_bundle_options_completionHandlerSelector = mkSelector "newTexturesWithNames:scaleFactor:bundle:options:completionHandler:"

-- | @Selector@ for @newTexturesWithNames:scaleFactor:displayGamut:bundle:options:completionHandler:@
newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandlerSelector :: Selector '[Id NSArray, CDouble, NSDisplayGamut, Id NSBundle, Id NSDictionary, Ptr ()] ()
newTexturesWithNames_scaleFactor_displayGamut_bundle_options_completionHandlerSelector = mkSelector "newTexturesWithNames:scaleFactor:displayGamut:bundle:options:completionHandler:"

-- | @Selector@ for @newTextureWithData:options:completionHandler:@
newTextureWithData_options_completionHandlerSelector :: Selector '[Id NSData, Id NSDictionary, Ptr ()] ()
newTextureWithData_options_completionHandlerSelector = mkSelector "newTextureWithData:options:completionHandler:"

-- | @Selector@ for @newTextureWithCGImage:options:completionHandler:@
newTextureWithCGImage_options_completionHandlerSelector :: Selector '[Ptr (), Id NSDictionary, Ptr ()] ()
newTextureWithCGImage_options_completionHandlerSelector = mkSelector "newTextureWithCGImage:options:completionHandler:"

-- | @Selector@ for @newTextureWithMDLTexture:options:completionHandler:@
newTextureWithMDLTexture_options_completionHandlerSelector :: Selector '[Id MDLTexture, Id NSDictionary, Ptr ()] ()
newTextureWithMDLTexture_options_completionHandlerSelector = mkSelector "newTextureWithMDLTexture:options:completionHandler:"

-- | @Selector@ for @newTextureWithContentsOfURL:options:error:@
newTextureWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] RawId
newTextureWithContentsOfURL_options_errorSelector = mkSelector "newTextureWithContentsOfURL:options:error:"

-- | @Selector@ for @newTexturesWithContentsOfURLs:options:error:@
newTexturesWithContentsOfURLs_options_errorSelector :: Selector '[Id NSArray, Id NSDictionary, Id NSError] (Id NSArray)
newTexturesWithContentsOfURLs_options_errorSelector = mkSelector "newTexturesWithContentsOfURLs:options:error:"

-- | @Selector@ for @newTextureWithData:options:error:@
newTextureWithData_options_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] RawId
newTextureWithData_options_errorSelector = mkSelector "newTextureWithData:options:error:"

-- | @Selector@ for @newTextureWithCGImage:options:error:@
newTextureWithCGImage_options_errorSelector :: Selector '[Ptr (), Id NSDictionary, Id NSError] RawId
newTextureWithCGImage_options_errorSelector = mkSelector "newTextureWithCGImage:options:error:"

-- | @Selector@ for @newTextureWithMDLTexture:options:error:@
newTextureWithMDLTexture_options_errorSelector :: Selector '[Id MDLTexture, Id NSDictionary, Id NSError] RawId
newTextureWithMDLTexture_options_errorSelector = mkSelector "newTextureWithMDLTexture:options:error:"

-- | @Selector@ for @newTextureWithName:scaleFactor:bundle:options:error:@
newTextureWithName_scaleFactor_bundle_options_errorSelector :: Selector '[Id NSString, CDouble, Id NSBundle, Id NSDictionary, Id NSError] RawId
newTextureWithName_scaleFactor_bundle_options_errorSelector = mkSelector "newTextureWithName:scaleFactor:bundle:options:error:"

-- | @Selector@ for @newTextureWithName:scaleFactor:displayGamut:bundle:options:error:@
newTextureWithName_scaleFactor_displayGamut_bundle_options_errorSelector :: Selector '[Id NSString, CDouble, NSDisplayGamut, Id NSBundle, Id NSDictionary, Id NSError] RawId
newTextureWithName_scaleFactor_displayGamut_bundle_options_errorSelector = mkSelector "newTextureWithName:scaleFactor:displayGamut:bundle:options:error:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

