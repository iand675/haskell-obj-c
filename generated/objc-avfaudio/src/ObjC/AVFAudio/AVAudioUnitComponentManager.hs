{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioUnitComponentManager
--
-- A singleton object that provides an easy way to find audio components that are			registered with the system.
--
-- AVAudioUnitComponentManager provides methods to search and query various information about the	audio components without opening them.
--
-- Currently audio components that are audio units can only be searched.
--
-- The class also supports predefined system tags and arbitrary user tags. Each audio unit can be	tagged as part of its definition. Refer to AudioComponent.h for more details. AudioUnit Hosts	such as Logic or GarageBand can present groupings of audio units based on the tags.
--
-- Searching for audio units can be done in various ways		- using a NSPredicate that contains search strings for tags or descriptions		- using a block to match on custom criteria		- using an AudioComponentDescription
--
-- Generated bindings for @AVAudioUnitComponentManager@.
module ObjC.AVFAudio.AVAudioUnitComponentManager
  ( AVAudioUnitComponentManager
  , IsAVAudioUnitComponentManager(..)
  , sharedAudioUnitComponentManager
  , componentsMatchingPredicate
  , componentsPassingTest
  , componentsMatchingDescription
  , tagNames
  , standardLocalizedTagNames
  , sharedAudioUnitComponentManagerSelector
  , componentsMatchingPredicateSelector
  , componentsPassingTestSelector
  , componentsMatchingDescriptionSelector
  , tagNamesSelector
  , standardLocalizedTagNamesSelector


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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AudioToolbox.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ sharedAudioUnitComponentManager@
sharedAudioUnitComponentManager :: IO (Id AVAudioUnitComponentManager)
sharedAudioUnitComponentManager  =
  do
    cls' <- getRequiredClass "AVAudioUnitComponentManager"
    sendClassMsg cls' (mkSelector "sharedAudioUnitComponentManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | componentsMatchingPredicate:
--
-- returns an array of AVAudioUnitComponent objects that match the search predicate.
--
-- AudioComponent's information or tags can be used to build a search criteria.  		For example, "typeName CONTAINS 'Effect'" or tags IN {'Sampler', 'MIDI'}"
--
-- ObjC selector: @- componentsMatchingPredicate:@
componentsMatchingPredicate :: (IsAVAudioUnitComponentManager avAudioUnitComponentManager, IsNSPredicate predicate) => avAudioUnitComponentManager -> predicate -> IO (Id NSArray)
componentsMatchingPredicate avAudioUnitComponentManager  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg avAudioUnitComponentManager (mkSelector "componentsMatchingPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | componentsPassingTest:
--
-- returns an array of AVAudioUnitComponent objects that pass the user provided block method.
--
-- For each AudioComponent found by the manager, the block method will be called. If the return 		value is YES then the AudioComponent is added to the resulting array else it will excluded.  		This gives more control to the block provider to filter out the components returned.
--
-- ObjC selector: @- componentsPassingTest:@
componentsPassingTest :: IsAVAudioUnitComponentManager avAudioUnitComponentManager => avAudioUnitComponentManager -> Ptr () -> IO (Id NSArray)
componentsPassingTest avAudioUnitComponentManager  testHandler =
  sendMsg avAudioUnitComponentManager (mkSelector "componentsPassingTest:") (retPtr retVoid) [argPtr (castPtr testHandler :: Ptr ())] >>= retainedObject . castPtr

-- | componentsMatchingDescription:
--
-- returns an array of AVAudioUnitComponent objects that match the description.
--
-- This method provides a mechanism to search for AudioComponents using AudioComponentDescription		structure. The type, subtype and manufacturer fields are used to search for audio units. A  		value of 0 for any of these fields is a wildcard and returns the first match found.
--
-- ObjC selector: @- componentsMatchingDescription:@
componentsMatchingDescription :: IsAVAudioUnitComponentManager avAudioUnitComponentManager => avAudioUnitComponentManager -> AudioComponentDescription -> IO (Id NSArray)
componentsMatchingDescription avAudioUnitComponentManager  desc =
  sendMsg avAudioUnitComponentManager (mkSelector "componentsMatchingDescription:") (retPtr retVoid) [argAudioComponentDescription desc] >>= retainedObject . castPtr

-- | returns all tags associated with the current user as well as all system tags defined by 		the audio unit(s).
--
-- ObjC selector: @- tagNames@
tagNames :: IsAVAudioUnitComponentManager avAudioUnitComponentManager => avAudioUnitComponentManager -> IO (Id NSArray)
tagNames avAudioUnitComponentManager  =
  sendMsg avAudioUnitComponentManager (mkSelector "tagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | returns the localized standard system tags defined by the audio unit(s).
--
-- ObjC selector: @- standardLocalizedTagNames@
standardLocalizedTagNames :: IsAVAudioUnitComponentManager avAudioUnitComponentManager => avAudioUnitComponentManager -> IO (Id NSArray)
standardLocalizedTagNames avAudioUnitComponentManager  =
  sendMsg avAudioUnitComponentManager (mkSelector "standardLocalizedTagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedAudioUnitComponentManager@
sharedAudioUnitComponentManagerSelector :: Selector
sharedAudioUnitComponentManagerSelector = mkSelector "sharedAudioUnitComponentManager"

-- | @Selector@ for @componentsMatchingPredicate:@
componentsMatchingPredicateSelector :: Selector
componentsMatchingPredicateSelector = mkSelector "componentsMatchingPredicate:"

-- | @Selector@ for @componentsPassingTest:@
componentsPassingTestSelector :: Selector
componentsPassingTestSelector = mkSelector "componentsPassingTest:"

-- | @Selector@ for @componentsMatchingDescription:@
componentsMatchingDescriptionSelector :: Selector
componentsMatchingDescriptionSelector = mkSelector "componentsMatchingDescription:"

-- | @Selector@ for @tagNames@
tagNamesSelector :: Selector
tagNamesSelector = mkSelector "tagNames"

-- | @Selector@ for @standardLocalizedTagNames@
standardLocalizedTagNamesSelector :: Selector
standardLocalizedTagNamesSelector = mkSelector "standardLocalizedTagNames"

