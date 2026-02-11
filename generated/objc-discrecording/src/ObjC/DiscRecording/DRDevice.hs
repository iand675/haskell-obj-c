{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DRDevice@.
module ObjC.DiscRecording.DRDevice
  ( DRDevice
  , IsDRDevice(..)
  , devices
  , deviceForBSDName
  , deviceForIORegistryEntryPath
  , isValid
  , info
  , status
  , openTray
  , closeTray
  , ejectMedia
  , acquireExclusiveAccess
  , releaseExclusiveAccess
  , acquireMediaReservation
  , releaseMediaReservation
  , isEqualToDevice
  , mediaIsPresent
  , mediaIsTransitioning
  , mediaIsBusy
  , mediaType
  , mediaIsBlank
  , mediaIsAppendable
  , mediaIsOverwritable
  , mediaIsErasable
  , mediaIsReserved
  , mediaSpaceOverwritable
  , mediaSpaceUsed
  , mediaSpaceFree
  , trayIsOpen
  , bsdName
  , writesCD
  , writesDVD
  , displayName
  , ioRegistryEntryPath
  , devicesSelector
  , deviceForBSDNameSelector
  , deviceForIORegistryEntryPathSelector
  , isValidSelector
  , infoSelector
  , statusSelector
  , openTraySelector
  , closeTraySelector
  , ejectMediaSelector
  , acquireExclusiveAccessSelector
  , releaseExclusiveAccessSelector
  , acquireMediaReservationSelector
  , releaseMediaReservationSelector
  , isEqualToDeviceSelector
  , mediaIsPresentSelector
  , mediaIsTransitioningSelector
  , mediaIsBusySelector
  , mediaTypeSelector
  , mediaIsBlankSelector
  , mediaIsAppendableSelector
  , mediaIsOverwritableSelector
  , mediaIsErasableSelector
  , mediaIsReservedSelector
  , mediaSpaceOverwritableSelector
  , mediaSpaceUsedSelector
  , mediaSpaceFreeSelector
  , trayIsOpenSelector
  , bsdNameSelector
  , writesCDSelector
  , writesDVDSelector
  , displayNameSelector
  , ioRegistryEntryPathSelector


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

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | devices
--
-- Obtains a static list of devices connected to the computer.
--
-- Returns all CD/DVD devices connected to the computer at the time this 					method is called. Since devices can come and go at any time, the output of this 					method is simply a snapshot of the set of devices connected.
--
-- Returns: An NSArray of DRDevices.
--
-- ObjC selector: @+ devices@
devices :: IO (Id NSArray)
devices  =
  do
    cls' <- getRequiredClass "DRDevice"
    sendClassMsg cls' (mkSelector "devices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | deviceForBSDName:
--
-- Obtains a DRDevice for the device corresponding to the bsd /dev node.
--
-- If the device is not an authoring device (i.e., CDR, CDRW, DVR-R, etc), returns nil.
--
-- @bsdName@ — The bsd /dev node name.
--
-- Returns: An autoreleased DRDevice object.
--
-- ObjC selector: @+ deviceForBSDName:@
deviceForBSDName :: IsNSString bsdName => bsdName -> IO (Id DRDevice)
deviceForBSDName bsdName =
  do
    cls' <- getRequiredClass "DRDevice"
    withObjCPtr bsdName $ \raw_bsdName ->
      sendClassMsg cls' (mkSelector "deviceForBSDName:") (retPtr retVoid) [argPtr (castPtr raw_bsdName :: Ptr ())] >>= retainedObject . castPtr

-- | deviceForIORegistryEntryPath:
--
-- Obtains a DRDevice for the device at the path.
--
-- If the device is not an authoring device (i.e., CDR, CDRW, DVR-R, etc), returns nil.
--
-- @path@ — The IORegistry path to the device you wish to obtain a DRDEvice for.
--
-- Returns: An autoreleased DRDevice object.
--
-- ObjC selector: @+ deviceForIORegistryEntryPath:@
deviceForIORegistryEntryPath :: IsNSString path => path -> IO (Id DRDevice)
deviceForIORegistryEntryPath path =
  do
    cls' <- getRequiredClass "DRDevice"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "deviceForIORegistryEntryPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | isValid
--
-- Returns whether or not the device represented by the receiver is still attached					to the computer.
--
-- Because of the way some physical interconnects work, a device					which is unplugged and replugged in does not necessarily look like the same					device to the computer and would be invalid in that instance.
--
-- Returns: Returns YES if device is valid and NO if not.
--
-- ObjC selector: @- isValid@
isValid :: IsDRDevice drDevice => drDevice -> IO Bool
isValid drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "isValid") retCULong []

-- | info
--
-- Returns a dictionary of information describing the device.
--
-- The information returned include the types of media the device can write to, how 					it's connected and its identifying information such as the vendor and product name.
--
-- Returns: An NSDictionary containing device information.
--
-- ObjC selector: @- info@
info :: IsDRDevice drDevice => drDevice -> IO (Id NSDictionary)
info drDevice  =
  sendMsg drDevice (mkSelector "info") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | status
--
-- Returns a dictionary of information describing the media in the device.
--
-- In addition to information about the media (type, space available/used, etc),					the dictionary returned includes those pieces of information about the device 					itself which are in part determined by the media (i.e., maximum burn speed).
--
-- Returns: An NSDictionary containing media information.
--
-- ObjC selector: @- status@
status :: IsDRDevice drDevice => drDevice -> IO (Id NSDictionary)
status drDevice  =
  sendMsg drDevice (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | openTray
--
-- Commands the device to open its tray.
--
-- Does nothing if the device does not have a tray (slotload). If there is media in					the drive this method will do nothing and return false. In this case use
--
-- //apple_ref/occ/instm/DRDevice/ejectMedia ejectMedia
--
-- to eject the media and open the tray.
--
-- Returns: Returns YES if the tray could be opened and NO if not.
--
-- ObjC selector: @- openTray@
openTray :: IsDRDevice drDevice => drDevice -> IO Bool
openTray drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "openTray") retCULong []

-- | closeTray
--
-- Commands the device to close its tray.
--
-- Does nothing if the device does not have a tray (slotload).
--
-- Returns: Returns YES if the tray could be closed and NO if not.
--
-- ObjC selector: @- closeTray@
closeTray :: IsDRDevice drDevice => drDevice -> IO Bool
closeTray drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "closeTray") retCULong []

-- | ejectMedia
--
-- Commands the device to eject the media.
--
-- This command first unmounts any volumes associated with the media and					then ejects the media from the drive. If the media could not be ejected, 					most likely this is because a volume associated with the media could not be unmounted.
--
-- Returns: Returns YES if the media could be ejected and NO if not.
--
-- ObjC selector: @- ejectMedia@
ejectMedia :: IsDRDevice drDevice => drDevice -> IO Bool
ejectMedia drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "ejectMedia") retCULong []

-- | acquireExclusiveAccess
--
-- Attempts to acquire an exclusive access session with the device.
--
-- Acquiring exclusive access to the device prevents any process					other than the one acquiring access from communicating with the					device. So once exclusive access is granted, the device is unusable by any other process. 					Because of this all volumes mounted from media in the drive must be unmounted					before exclusive access can be granted.
--
-- Exclusive access can be acquired multiple times. Each time this method					is called, a call to
--
-- //apple_ref/occ/instm/DRDevice/releaseExclusiveAccess releaseExclusiveAccess
--
-- > must be made at a later time,					otherwise the process will never release its exclusive access.
--
-- Returns: Returns YES if the exclusinve access is acquired and NO if not.
--
-- ObjC selector: @- acquireExclusiveAccess@
acquireExclusiveAccess :: IsDRDevice drDevice => drDevice -> IO Bool
acquireExclusiveAccess drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "acquireExclusiveAccess") retCULong []

-- | releaseExclusiveAccess
--
-- Releases the latest exclusive access request for the device.
--
-- A call to this method must be made for every call to
--
-- //apple_ref/occ/instm/DRDevice/acquireExclusiveAccess acquireExclusiveAccess
--
-- , 					otherwise the process will never release its exclusive access.
--
-- ObjC selector: @- releaseExclusiveAccess@
releaseExclusiveAccess :: IsDRDevice drDevice => drDevice -> IO ()
releaseExclusiveAccess drDevice  =
  sendMsg drDevice (mkSelector "releaseExclusiveAccess") retVoid []

-- | acquireMediaReservation
--
-- Indicate an interest in the blank media reservation.
--
-- Blank media participates in a reservation system that allows applications to express					their claim on blank media to other applications.  Indicating an interest in the reservation					isn't enough to assume it's been acquired, as there are likely to be other applications in					the system who have also indicated an interest in the blank media reservation.  You will					receive a
--
-- DRDeviceStatusChangedNotification DRDeviceStatusChangedNotification
--
-- with a value of TRUE for the
--
-- DRDeviceMediaIsReservedKey DRDeviceMediaIsReservedKey
--
-- when the blank media reservation has been acquired.
--
-- This function may be called multiple times. Each time it is called, a call to
--
-- //apple_ref/occ/instm/DRDevice/releaseMediaReservation releaseMediaReservation
--
-- must be made at a later time, otherwise the process will					never fully rescind its interest in the blank media reservation.
--
-- ObjC selector: @- acquireMediaReservation@
acquireMediaReservation :: IsDRDevice drDevice => drDevice -> IO ()
acquireMediaReservation drDevice  =
  sendMsg drDevice (mkSelector "acquireMediaReservation") retVoid []

-- | releaseMediaReservation
--
-- Releases any media reservation that might be in place for the device.
--
-- If media is inserted and reserved, then the reservation will be passed 					on to the next process with a reservation request.
--
-- ObjC selector: @- releaseMediaReservation@
releaseMediaReservation :: IsDRDevice drDevice => drDevice -> IO ()
releaseMediaReservation drDevice  =
  sendMsg drDevice (mkSelector "releaseMediaReservation") retVoid []

-- | isEqualToDevice:
--
-- Compares the receiver to another device.
--
-- @otherDevice@ — Another DRDevice instance to compare to the receiver.
--
-- Returns: Returns YES if the receiver is equal to otherDevice.
--
-- ObjC selector: @- isEqualToDevice:@
isEqualToDevice :: (IsDRDevice drDevice, IsDRDevice otherDevice) => drDevice -> otherDevice -> IO Bool
isEqualToDevice drDevice  otherDevice =
withObjCPtr otherDevice $ \raw_otherDevice ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "isEqualToDevice:") retCULong [argPtr (castPtr raw_otherDevice :: Ptr ())]

-- | mediaIsPresent
--
-- Reports the presence of the media.
--
-- ObjC selector: @- mediaIsPresent@
mediaIsPresent :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsPresent drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsPresent") retCULong []

-- | mediaIsTransitioning
--
-- Returns YES if the media is in transition (spinning up or down for example).
--
-- ObjC selector: @- mediaIsTransitioning@
mediaIsTransitioning :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsTransitioning drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsTransitioning") retCULong []

-- | mediaIsBusy
--
-- Returns YES if the media is in use by some process - even the one				making this call.
--
-- ObjC selector: @- mediaIsBusy@
mediaIsBusy :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsBusy drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsBusy") retCULong []

-- | mediaType
--
-- Returns the type of media currently inserted into the device.
--
-- ObjC selector: @- mediaType@
mediaType :: IsDRDevice drDevice => drDevice -> IO (Id NSString)
mediaType drDevice  =
  sendMsg drDevice (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaIsBlank
--
-- Returns YES the media in the device is blank.
--
-- ObjC selector: @- mediaIsBlank@
mediaIsBlank :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsBlank drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsBlank") retCULong []

-- | mediaIsAppendable
--
-- Returns YES if the media in the device can have more data appended to				any existing data.
--
-- ObjC selector: @- mediaIsAppendable@
mediaIsAppendable :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsAppendable drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsAppendable") retCULong []

-- | mediaIsOverwritable
--
-- Returns YES if the media in the device can be fully (re)written.
--
-- ObjC selector: @- mediaIsOverwritable@
mediaIsOverwritable :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsOverwritable drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsOverwritable") retCULong []

-- | mediaIsErasable
--
-- Returns YES if the media can be erased (i.e., CD-RW, DVD-RW, etc).
--
-- ObjC selector: @- mediaIsErasable@
mediaIsErasable :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsErasable drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsErasable") retCULong []

-- | mediaIsReserved
--
-- Returns YES if the application calling this method currently holds				the reservation on the media.
--
-- ObjC selector: @- mediaIsReserved@
mediaIsReserved :: IsDRDevice drDevice => drDevice -> IO Bool
mediaIsReserved drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "mediaIsReserved") retCULong []

-- | mediaSpaceOverwritable
--
-- Returns the amount of writable space on the media.
--
-- ObjC selector: @- mediaSpaceOverwritable@
mediaSpaceOverwritable :: IsDRDevice drDevice => drDevice -> IO (Id DRMSF)
mediaSpaceOverwritable drDevice  =
  sendMsg drDevice (mkSelector "mediaSpaceOverwritable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaSpaceUsed
--
-- Returns the amount of used space on the media.
--
-- ObjC selector: @- mediaSpaceUsed@
mediaSpaceUsed :: IsDRDevice drDevice => drDevice -> IO (Id DRMSF)
mediaSpaceUsed drDevice  =
  sendMsg drDevice (mkSelector "mediaSpaceUsed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mediaSpaceFree
--
-- Returns the amount of free space on the media.
--
-- ObjC selector: @- mediaSpaceFree@
mediaSpaceFree :: IsDRDevice drDevice => drDevice -> IO (Id DRMSF)
mediaSpaceFree drDevice  =
  sendMsg drDevice (mkSelector "mediaSpaceFree") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | trayIsOpen
--
-- Reports the tray state of the device.
--
-- Returns: Returns YES if the device has a tray and it is open.
--
-- ObjC selector: @- trayIsOpen@
trayIsOpen :: IsDRDevice drDevice => drDevice -> IO Bool
trayIsOpen drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "trayIsOpen") retCULong []

-- | bsdName
--
-- Returns the bsd /dev node name.
--
-- ObjC selector: @- bsdName@
bsdName :: IsDRDevice drDevice => drDevice -> IO (Id NSString)
bsdName drDevice  =
  sendMsg drDevice (mkSelector "bsdName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | writesCD
--
-- Reports the device's ability to burn to CD-type media.
--
-- Returns: Returns YES if the device has the ability to write to CD-R media.
--
-- ObjC selector: @- writesCD@
writesCD :: IsDRDevice drDevice => drDevice -> IO Bool
writesCD drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "writesCD") retCULong []

-- | writesDVD
--
-- Reports the device's ability to burn to DVD-type media.
--
-- Returns: Returns YES if the device has the ability to write to DVD media.
--
-- ObjC selector: @- writesDVD@
writesDVD :: IsDRDevice drDevice => drDevice -> IO Bool
writesDVD drDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg drDevice (mkSelector "writesDVD") retCULong []

-- | displayName
--
-- Returns an NSString suitable for display in the user interface.
--
-- ObjC selector: @- displayName@
displayName :: IsDRDevice drDevice => drDevice -> IO (Id NSString)
displayName drDevice  =
  sendMsg drDevice (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ioRegistryEntryPath
--
-- Returns the path to the device in the IO Registry.
--
-- ObjC selector: @- ioRegistryEntryPath@
ioRegistryEntryPath :: IsDRDevice drDevice => drDevice -> IO (Id NSString)
ioRegistryEntryPath drDevice  =
  sendMsg drDevice (mkSelector "ioRegistryEntryPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @devices@
devicesSelector :: Selector
devicesSelector = mkSelector "devices"

-- | @Selector@ for @deviceForBSDName:@
deviceForBSDNameSelector :: Selector
deviceForBSDNameSelector = mkSelector "deviceForBSDName:"

-- | @Selector@ for @deviceForIORegistryEntryPath:@
deviceForIORegistryEntryPathSelector :: Selector
deviceForIORegistryEntryPathSelector = mkSelector "deviceForIORegistryEntryPath:"

-- | @Selector@ for @isValid@
isValidSelector :: Selector
isValidSelector = mkSelector "isValid"

-- | @Selector@ for @info@
infoSelector :: Selector
infoSelector = mkSelector "info"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @openTray@
openTraySelector :: Selector
openTraySelector = mkSelector "openTray"

-- | @Selector@ for @closeTray@
closeTraySelector :: Selector
closeTraySelector = mkSelector "closeTray"

-- | @Selector@ for @ejectMedia@
ejectMediaSelector :: Selector
ejectMediaSelector = mkSelector "ejectMedia"

-- | @Selector@ for @acquireExclusiveAccess@
acquireExclusiveAccessSelector :: Selector
acquireExclusiveAccessSelector = mkSelector "acquireExclusiveAccess"

-- | @Selector@ for @releaseExclusiveAccess@
releaseExclusiveAccessSelector :: Selector
releaseExclusiveAccessSelector = mkSelector "releaseExclusiveAccess"

-- | @Selector@ for @acquireMediaReservation@
acquireMediaReservationSelector :: Selector
acquireMediaReservationSelector = mkSelector "acquireMediaReservation"

-- | @Selector@ for @releaseMediaReservation@
releaseMediaReservationSelector :: Selector
releaseMediaReservationSelector = mkSelector "releaseMediaReservation"

-- | @Selector@ for @isEqualToDevice:@
isEqualToDeviceSelector :: Selector
isEqualToDeviceSelector = mkSelector "isEqualToDevice:"

-- | @Selector@ for @mediaIsPresent@
mediaIsPresentSelector :: Selector
mediaIsPresentSelector = mkSelector "mediaIsPresent"

-- | @Selector@ for @mediaIsTransitioning@
mediaIsTransitioningSelector :: Selector
mediaIsTransitioningSelector = mkSelector "mediaIsTransitioning"

-- | @Selector@ for @mediaIsBusy@
mediaIsBusySelector :: Selector
mediaIsBusySelector = mkSelector "mediaIsBusy"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @mediaIsBlank@
mediaIsBlankSelector :: Selector
mediaIsBlankSelector = mkSelector "mediaIsBlank"

-- | @Selector@ for @mediaIsAppendable@
mediaIsAppendableSelector :: Selector
mediaIsAppendableSelector = mkSelector "mediaIsAppendable"

-- | @Selector@ for @mediaIsOverwritable@
mediaIsOverwritableSelector :: Selector
mediaIsOverwritableSelector = mkSelector "mediaIsOverwritable"

-- | @Selector@ for @mediaIsErasable@
mediaIsErasableSelector :: Selector
mediaIsErasableSelector = mkSelector "mediaIsErasable"

-- | @Selector@ for @mediaIsReserved@
mediaIsReservedSelector :: Selector
mediaIsReservedSelector = mkSelector "mediaIsReserved"

-- | @Selector@ for @mediaSpaceOverwritable@
mediaSpaceOverwritableSelector :: Selector
mediaSpaceOverwritableSelector = mkSelector "mediaSpaceOverwritable"

-- | @Selector@ for @mediaSpaceUsed@
mediaSpaceUsedSelector :: Selector
mediaSpaceUsedSelector = mkSelector "mediaSpaceUsed"

-- | @Selector@ for @mediaSpaceFree@
mediaSpaceFreeSelector :: Selector
mediaSpaceFreeSelector = mkSelector "mediaSpaceFree"

-- | @Selector@ for @trayIsOpen@
trayIsOpenSelector :: Selector
trayIsOpenSelector = mkSelector "trayIsOpen"

-- | @Selector@ for @bsdName@
bsdNameSelector :: Selector
bsdNameSelector = mkSelector "bsdName"

-- | @Selector@ for @writesCD@
writesCDSelector :: Selector
writesCDSelector = mkSelector "writesCD"

-- | @Selector@ for @writesDVD@
writesDVDSelector :: Selector
writesDVDSelector = mkSelector "writesDVD"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @ioRegistryEntryPath@
ioRegistryEntryPathSelector :: Selector
ioRegistryEntryPathSelector = mkSelector "ioRegistryEntryPath"

