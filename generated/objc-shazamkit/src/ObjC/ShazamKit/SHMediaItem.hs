{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents the metadata for a reference signature.
--
-- This class uses subscripting for the data elements of a custom media item that an existing property doesn't already represent.
--
-- Add a readable custom property by extending ``SHMediaItemProperty-struct``  with a key for that property, and by extending this class with a property that uses the key. The following code shows the extensions for an episode number:
--
-- ```swift // Add an episode number to the list of properties. extension SHMediaItemProperty {     static let episode = SHMediaItemProperty("Episode") }
--
-- // Add a property for returning the episode number using a subscript. extension SHMediaItem {     var episode: Int? {         return self[.episode] as? Int     } } ```
--
-- Add your custom property when you create the media item as the following code shows:
--
-- ```swift // Create a new media item and set the title, subtitle, and episode properties. let mediaItem = SHMediaItem(properties: [.episode: 42,                                          .title: "Question",                                          .subtitle: "The Answer"]) ```
--
-- > Note: > The class of the object that represents a custom object must be one of: @Dictionary@, @Array@, @URL@, @Number@, @String@, @Date@, or @Data@.
--
-- Generated bindings for @SHMediaItem@.
module ObjC.ShazamKit.SHMediaItem
  ( SHMediaItem
  , IsSHMediaItem(..)
  , mediaItemWithProperties
  , fetchMediaItemWithShazamID_completionHandler
  , valueForProperty
  , objectForKeyedSubscript
  , new
  , init_
  , shazamID
  , title
  , subtitle
  , artist
  , genres
  , appleMusicID
  , appleMusicURL
  , webURL
  , artworkURL
  , videoURL
  , explicitContent
  , isrc
  , creationDate
  , mediaItemWithPropertiesSelector
  , fetchMediaItemWithShazamID_completionHandlerSelector
  , valueForPropertySelector
  , objectForKeyedSubscriptSelector
  , newSelector
  , initSelector
  , shazamIDSelector
  , titleSelector
  , subtitleSelector
  , artistSelector
  , genresSelector
  , appleMusicIDSelector
  , appleMusicURLSelector
  , webURLSelector
  , artworkURLSelector
  , videoURLSelector
  , explicitContentSelector
  , isrcSelector
  , creationDateSelector


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

-- | Creates a media item object with a dictionary of properties and their associated values.
--
-- - Parameters:   - properties: A dictionary that contains the media item properties and their associated values.
--
-- ObjC selector: @+ mediaItemWithProperties:@
mediaItemWithProperties :: IsNSDictionary properties => properties -> IO (Id SHMediaItem)
mediaItemWithProperties properties =
  do
    cls' <- getRequiredClass "SHMediaItem"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "mediaItemWithProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | Requests the media item for the song with the specified Shazam ID.
--
-- > Important: > You can call this method from synchronous code using a completion handler, as shown on this page, or you can call it as an asynchronous method that has the following declaration: > > ```swift > class func fetch(shazamID: String) async throws -> SHMediaItem > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- - Parameters:   - shazamID: The Shazam ID of the song.   - completionHandler: The completion handler that the system calls with the result of the request.
--
-- This block takes the following parameters:
--
-- - term @mediaItem@: A media item.     - term @error@: An error object if a problem occurs when fetching the media item; otherwise, @nil@.
--
-- ObjC selector: @+ fetchMediaItemWithShazamID:completionHandler:@
fetchMediaItemWithShazamID_completionHandler :: IsNSString shazamID => shazamID -> Ptr () -> IO ()
fetchMediaItemWithShazamID_completionHandler shazamID completionHandler =
  do
    cls' <- getRequiredClass "SHMediaItem"
    withObjCPtr shazamID $ \raw_shazamID ->
      sendClassMsg cls' (mkSelector "fetchMediaItemWithShazamID:completionHandler:") retVoid [argPtr (castPtr raw_shazamID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Accesses the property for the specified key for reading.
--
-- - Parameters:   - property: The key for the property.
--
-- - Returns: The value of the property; otherwise, @nil@.
--
-- ObjC selector: @- valueForProperty:@
valueForProperty :: (IsSHMediaItem shMediaItem, IsNSString property) => shMediaItem -> property -> IO RawId
valueForProperty shMediaItem  property =
withObjCPtr property $ \raw_property ->
    fmap (RawId . castPtr) $ sendMsg shMediaItem (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ())]

-- | Accesses the property for the specified key for reading.
--
-- - Parameters:   - key: The key for the media item property.
--
-- - Returns: The value of the property; otherwise, @nil@.
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsSHMediaItem shMediaItem, IsNSString key) => shMediaItem -> key -> IO RawId
objectForKeyedSubscript shMediaItem  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg shMediaItem (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @+ new@
new :: IO (Id SHMediaItem)
new  =
  do
    cls' <- getRequiredClass "SHMediaItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id SHMediaItem)
init_ shMediaItem  =
  sendMsg shMediaItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Shazam ID for the song.
--
-- ObjC selector: @- shazamID@
shazamID :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
shazamID shMediaItem  =
  sendMsg shMediaItem (mkSelector "shazamID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A title for the media item.
--
-- ObjC selector: @- title@
title :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
title shMediaItem  =
  sendMsg shMediaItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A subtitle for the media item.
--
-- ObjC selector: @- subtitle@
subtitle :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
subtitle shMediaItem  =
  sendMsg shMediaItem (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the artist for the media item, such as the performer of a song.
--
-- ObjC selector: @- artist@
artist :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
artist shMediaItem  =
  sendMsg shMediaItem (mkSelector "artist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of genre names for the media item.
--
-- The array is empty if there are no media items.
--
-- ObjC selector: @- genres@
genres :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSArray)
genres shMediaItem  =
  sendMsg shMediaItem (mkSelector "genres") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Apple Music ID for the song.
--
-- ObjC selector: @- appleMusicID@
appleMusicID :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
appleMusicID shMediaItem  =
  sendMsg shMediaItem (mkSelector "appleMusicID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A link to the Apple Music page that contains the full information for the song.
--
-- ObjC selector: @- appleMusicURL@
appleMusicURL :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSURL)
appleMusicURL shMediaItem  =
  sendMsg shMediaItem (mkSelector "appleMusicURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A link to the Shazam Music catalog page that contains the full information for the song.
--
-- This link opens the Shazam app or App Clip if it's available on the device.
--
-- ObjC selector: @- webURL@
webURL :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSURL)
webURL shMediaItem  =
  sendMsg shMediaItem (mkSelector "webURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL for artwork for the media item, such as an album cover.
--
-- ObjC selector: @- artworkURL@
artworkURL :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSURL)
artworkURL shMediaItem  =
  sendMsg shMediaItem (mkSelector "artworkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The URL for a video for the media item, such as a music video.
--
-- ObjC selector: @- videoURL@
videoURL :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSURL)
videoURL shMediaItem  =
  sendMsg shMediaItem (mkSelector "videoURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the media item contains explicit content.
--
-- ObjC selector: @- explicitContent@
explicitContent :: IsSHMediaItem shMediaItem => shMediaItem -> IO Bool
explicitContent shMediaItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg shMediaItem (mkSelector "explicitContent") retCULong []

-- | The International Standard Recording Code (ISRC) for the media item.
--
-- ObjC selector: @- isrc@
isrc :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSString)
isrc shMediaItem  =
  sendMsg shMediaItem (mkSelector "isrc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date the media item was created.
--
-- ObjC selector: @- creationDate@
creationDate :: IsSHMediaItem shMediaItem => shMediaItem -> IO (Id NSDate)
creationDate shMediaItem  =
  sendMsg shMediaItem (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mediaItemWithProperties:@
mediaItemWithPropertiesSelector :: Selector
mediaItemWithPropertiesSelector = mkSelector "mediaItemWithProperties:"

-- | @Selector@ for @fetchMediaItemWithShazamID:completionHandler:@
fetchMediaItemWithShazamID_completionHandlerSelector :: Selector
fetchMediaItemWithShazamID_completionHandlerSelector = mkSelector "fetchMediaItemWithShazamID:completionHandler:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @shazamID@
shazamIDSelector :: Selector
shazamIDSelector = mkSelector "shazamID"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

-- | @Selector@ for @genres@
genresSelector :: Selector
genresSelector = mkSelector "genres"

-- | @Selector@ for @appleMusicID@
appleMusicIDSelector :: Selector
appleMusicIDSelector = mkSelector "appleMusicID"

-- | @Selector@ for @appleMusicURL@
appleMusicURLSelector :: Selector
appleMusicURLSelector = mkSelector "appleMusicURL"

-- | @Selector@ for @webURL@
webURLSelector :: Selector
webURLSelector = mkSelector "webURL"

-- | @Selector@ for @artworkURL@
artworkURLSelector :: Selector
artworkURLSelector = mkSelector "artworkURL"

-- | @Selector@ for @videoURL@
videoURLSelector :: Selector
videoURLSelector = mkSelector "videoURL"

-- | @Selector@ for @explicitContent@
explicitContentSelector :: Selector
explicitContentSelector = mkSelector "explicitContent"

-- | @Selector@ for @isrc@
isrcSelector :: Selector
isrcSelector = mkSelector "isrc"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

