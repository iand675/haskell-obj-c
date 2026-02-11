{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents the user's Shazam library.
--
-- Use @SHMediaLibrary@ to add matched songs from the Shazam catalog to the user's Shazam library.
--
-- > Note: > There's no system permission necessary to write to the user's Shazam library. Consider requesting permission from the user before adding songs to the library.
--
-- Generated bindings for @SHMediaLibrary@.
module ObjC.ShazamKit.SHMediaLibrary
  ( SHMediaLibrary
  , IsSHMediaLibrary(..)
  , addMediaItems_completionHandler
  , init_
  , new
  , defaultLibrary
  , addMediaItems_completionHandlerSelector
  , initSelector
  , newSelector
  , defaultLibrarySelector


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
import ObjC.Foundation.Internal.Classes

-- | Adds an array of songs to the user's Shazam library.
--
-- > Important: > You can call this method from synchronous code using a completion handler, as shown on this page, or you can call it as an asynchronous method that has the following declaration: > > ```swift > func add(_ mediaItems: [SHMediaItem]) async throws > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- Saving a song to the user's Shazam library also saves the following media item properties and their associated values:
--
-- - ``SHMediaItemProperty/shazamID`` - ``SHMediaItemProperty/title`` - ``SHMediaItemProperty/subtitle``, or ``SHMediaItemProperty/artist`` if the subtitle is unavailable
--
-- > Note: > Saving to the user's Shazam library works only for songs with a valid ``SHMediaItemProperty/shazamID``.
--
-- - Parameters:   - mediaItems: An array of media items that represents the songs to add to the library.   - completionHandler: The system calls this completion block after adding the media items to the library.
--
-- This block takes the following parameters:
--
-- - term @error@: An error object if a problem occurs when adding any item; otherwise, @nil@.
--
-- ObjC selector: @- addMediaItems:completionHandler:@
addMediaItems_completionHandler :: (IsSHMediaLibrary shMediaLibrary, IsNSArray mediaItems) => shMediaLibrary -> mediaItems -> Ptr () -> IO ()
addMediaItems_completionHandler shMediaLibrary  mediaItems completionHandler =
withObjCPtr mediaItems $ \raw_mediaItems ->
    sendMsg shMediaLibrary (mkSelector "addMediaItems:completionHandler:") retVoid [argPtr (castPtr raw_mediaItems :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsSHMediaLibrary shMediaLibrary => shMediaLibrary -> IO (Id SHMediaLibrary)
init_ shMediaLibrary  =
  sendMsg shMediaLibrary (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SHMediaLibrary)
new  =
  do
    cls' <- getRequiredClass "SHMediaLibrary"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An instance of the user's default Shazam library.
--
-- ObjC selector: @+ defaultLibrary@
defaultLibrary :: IO (Id SHMediaLibrary)
defaultLibrary  =
  do
    cls' <- getRequiredClass "SHMediaLibrary"
    sendClassMsg cls' (mkSelector "defaultLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addMediaItems:completionHandler:@
addMediaItems_completionHandlerSelector :: Selector
addMediaItems_completionHandlerSelector = mkSelector "addMediaItems:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @defaultLibrary@
defaultLibrarySelector :: Selector
defaultLibrarySelector = mkSelector "defaultLibrary"

