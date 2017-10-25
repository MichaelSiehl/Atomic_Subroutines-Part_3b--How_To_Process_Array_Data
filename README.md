# Atomic_Subroutines-Part_3b--How_To_Process_Array_Data
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines - Part 3b: How to process array data - allow for safe remote communication among a number of coarray images. 

# Overview
This GitHub repository contains an example program that processes array data remotely through Fortran 2008 atomic subroutines (atomic_define). The example program is a modification and extension of part 3a that can be found here: https://github.com/MichaelSiehl/Atomic_Subroutines-Part_3a--How_To_Process_Array_Data.<br />
This time, I did implement a first (primitive) version of a customized synchronization procedure to process the  remote array data transfer from several coarray images (2,3,4) to a single receiving coarray image (1).<br />

# How it works
The relevant codes are in the Main.f90 and OOOPimsc_admImageStatus_CA.f90 source code files.<br />
The example program should be compiled and run with 4 coarray images using OpenCoarrays/Gfortran.<br />
As with the example program from part 3a, we do a remote transfer of the array data from coarray images 2, 3, and 4 to the same receiving coarray image 1. (On the coarray images 2, 3, and 4, we do fill the array with values 2, 3, and 4 resp.).<br />
See this output from a program run:<br />
```fortran
 remote array transfer done: on image / array data           1 /           2           2           2           2           2
 remote array transfer done: on image / array data           1 /           3           3           3           3           3
 remote array transfer done: on image / array data           1 /           4           4           4           4           4
```

On the receiving coarray image 1, we do execute the newly created customized synchronization procedure do synchronize the array data transfers from the several sending images (2,3,4).<br />
Below is the code of the modified main program:<br />

```fortran
program Main
  use OOOGglob_Globals
  use OOOEerro_admError
  use OOOPimsc_admImageStatus_CA
  implicit none
  !
  integer(OOOGglob_kint) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:3) :: intA_RemoteImageNumbers ! please compile and run the
                                                                     ! program with 4 coarray images
  integer(OOOGglob_kint) :: intSetFromImageNumber
  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint), dimension (1:3,1:5) :: intA_TestArrayForRemoteReceive
  integer(OOOGglob_kint) :: intTestArrayUpperBound
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intA
  !
  intTestArrayUpperBound = ubound(intA_TestArrayForRemoteTransfer, 1)
  !
  !********************************* execute on coarray image 1:
  !
  if (this_image() == 1) then
    intNumberOfRemoteImages = 3
    intA_RemoteImageNumbers = (/2,3,4/)
    !
    ! initiate and wait for remote transfer of the TestArray data:
    call OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (OOOPimscImageStatus_CA_1, intTestArrayUpperBound, &
                      intA_TestArrayForRemoteReceive, intNumberOfRemoteImages, intA_RemoteImageNumbers)
    !
    do intCount = 1, intNumberOfRemoteImages
      write(*,*) 'remote array transfer done: on image / array data', &
                             this_image(), '/', intA_TestArrayForRemoteReceive(intCount,:)
    end do
    !
  !*********************************** execute on coarray image 2,3,and 4:
  else ! this_image > 1
    intNumberOfRemoteImages = 1
    intA_RemoteImageNumbers(1) = 1
    intA = this_image()
    intA_TestArrayForRemoteTransfer = (/intA,intA,intA,intA,intA/) ! fill the TestArray with some data locally
    !
    ! synchronize and distribute the TestArray data to the remote coarray image 1:
    call OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (OOOPimscImageStatus_CA_1, &
            intNumberOfRemoteImages, intA_RemoteImageNumbers, intTestArrayUpperBound, intA_TestArrayForRemoteTransfer)
    !
  !************************************
  end if
  !
end program Main
```

# - Code changes in the OOOPimsc_admImageStatus_CA module
1. The newly added customized synchronization procedure to synchronize the remote array data transfer from several coarray images to a single receiving coarray image:<br />
```fortran
subroutine OOOPimscSpinWaitBulkArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                  intNumberOfRemoteImages, intA_RemoteImageNumbers, intArrayUpperBound, &
                  intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, logExecuteSyncMemory)
  ! This routine is for atomic array bulk synchronization (among the executing image and multiple remote images)
  ! using a spin-wait loop synchronizaton. The routine synchronizes the array transfer from each involved remote image.
  ! Thus, the procedure implements a customized synchronization routine using atomic subroutines and
  ! the sync memory statement. Ordered execution segments between the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckArrayElementSyncStat
  integer(OOOGglob_kint), intent (in) :: intNumberOfRemoteImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (intNumberOfRemoteImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intArrayIndex
  integer(OOOGglob_kint), intent (in) :: intArrayUpperBound
  integer(OOOGglob_kint), dimension(1) :: intA_MaxVal
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfRemoteImages, 1:intArrayUpperBound, 1:2), &
                             intent (out) :: intA_ArrayElementSyncStatAndItsAdditionalAtomicValue
  integer(OOOGglob_kint), dimension (1:intArrayUpperBound, 1:2) &
                                  :: intA_ArrayElementSyncStatAndItsAdditionalAtomicVal
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intCount2
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitBulkArrayRang1Sync_atomic_intTestArray_CA")
                                                                !
                                                                ! check if the intArrayUpperBound argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intA_MaxVal(1) = ubound(Object_CA % &
                                                                              mA_atomic_intTestArray,1)
                                                                if (intArrayUpperBound .gt. intA_MaxVal(1)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayUpperBound is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  !**********************************************************************
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  !**********************************************************************
  ! wait (block) until all the array transfers from the distinct remote images  are in state intCheckArrayElementSyncStat
  ! spin-wait loop synchronization:
  do intCount = 1, intNumberOfRemoteImages
    intRemoteImageNumber = intA_RemoteImageNumbers(intCount)
                                                           ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intRemoteImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
    !
    call OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intCheckArrayElementSyncStat, &
                    intRemoteImageNumber, intArrayUpperBound, &
                    intA_ArrayElementSyncStatAndItsAdditionalAtomicVal, &
                    logExecuteSyncMemory = .false.)
    !
    intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(intCount,:,:) = &
                               intA_ArrayElementSyncStatAndItsAdditionalAtomicVal (:,:)
  !
  end do
  !
  if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitBulkArrayRang1Sync_atomic_intTestArray_CA
```
This newly created spin wait bulk synchronization procedure (for array data transfer among several coarray images) makes use of the synchronization procedure for array data transfer between two coarray images 'OOOPimscSpinWaitArrayRang1Sync_atomic_intTestArray_CA', that was developed previously.<br />
<br />
2. Another small but important extension was with the accessor of the derived type coarray array component ('TestArray'):<br />
```fortran
subroutine OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                        intImageNumber, intArrayIndex, logExecuteSyncMemory, intRemoteChannelNumberForLocalAtomicSet)
.
  integer(OOOGglob_kint), optional, intent (in) :: intRemoteChannelNumberForLocalAtomicSet
.
    if (present(intRemoteChannelNumberForLocalAtomicSet)) then
      ! do locally (!) write to the data transfer channel of a remote image:
      ! (usually to reset a value locally to prepare for a remote synchronization)
      call atomic_define(Object_CA % mA_atomic_intTestArray(intRemoteChannelNumberForLocalAtomicSet,intArrayIndex), &
                                                                intArrayElementSyncStat)
    else ! do locally write to the data transfer channel of this image:
      call atomic_define(Object_CA % mA_atomic_intTestArray(this_image(),intArrayIndex), intArrayElementSyncStat)
    end if
 .
 end subroutine
```
The newly added 'intRemoteChannelNumberForLocalAtomicSet' (optional) argument allows to locally write a value to the data transfer channel of a remote image. This is required to prepare the local image for an upcoming synchronization. (It was not necessarily required with the simple example program here, but rather with a slightly more sophisticated code structure).<br />
<br />
3. And finally, the two procedures containing the parallel logic codes for steering the remote array transfer below:
```fortran
! executed on coarray images 2,3,4:
!___________________________________________________________
!
! public
subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (Object_CA, intNumberOfImages, intA_RemoteImageNumbers, &
                                                intTestArrayUpperBound, intA_TestArrayForRemoteTransfer)
  ! This routine is to synchronize and distribute the TestArray (using atomic subroutines)
  ! to the involved remote image (1).
  ! To do so, this routine gets executed on separate coarray image(s)
  ! (on images 2,3,4 with this example)
  !
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint), intent (in) :: intTestArrayUpperBound
  integer(OOOGglob_kint), dimension (1:intTestArrayUpperBound), intent (in) :: intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intCount2
  integer(OOOGglob_kint) :: intImageNumber
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint), dimension (1:intTestArrayUpperBound) :: intA_TestArray ! will contain the packed enum values
                                                            ! for remote transfer
  integer(OOOGglob_kint) :: intEnumStepWidth
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeAndDistributeTheTestArray_CA")
  !
  !************************************************
  ! (1) wait until all the involved remote image (image 1 only here) do signal that they are
  ! in state WaitForTestArrayTransfer:
  ! (counterpart routine is step 2 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !*************************************************
  ! (2) pack and fill the TestArray locally with values from intA_TestArrayForRemoteTransfer:
  ! - Firstly, set this to allow for later synchronization of the array elements on the remote images:
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  ! - pack the Enum value with input values from intA_TestArrayForRemoteTransfer and fill the
  !   intA_TestArray elements with these values:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  do intCount = 1, intTestArrayUpperBound
    call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                        intA_TestArrayForRemoteTransfer(intCount), intPackedEnumValue, intEnumStepWidth)
    intA_TestArray(intCount) = intPackedEnumValue
  end do
  !
  !**********************************************************************
  ! (3) distribute the TestArray to the TestArray coarray components of the involved remote images (image 1 only here):
  ! (counterpart synchronization routine is step 3 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages ! distribute the array to each involved remote image
     intImageNumber = intA_RemoteImageNumbers(intCount)
    do intCount2 = 1, intTestArrayUpperBound ! distribute the array elements each individually:
      call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intA_TestArray(intCount2), &
                            intImageNumber, intArrayIndex = intCount2, logExecuteSyncMemory = .false.)
    end do
  end do
  !
  !************************************************
  ! (4) wait until all the involved remote image(s) (image 1 only here) do signal that they are
  ! in state TestArrayRemoteTransferDone:
  ! (counterpart routine is step 4 in OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone
  ! - spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !
  !**********************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
!  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
!                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !

                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeAndDistributeTheTestArray_CA
```

```fortran
! executed on coarray image 1:
!______________________________________________________________
!
! public
subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (Object_CA, intTestArrayUpperBound, &
                              intA_TestArrayForRemoteTransfer, intNumberOfRemoteImages, intA_RemoteImageNumbers)
  ! This routine is to synchronize and receive the TestArray (using atomic subroutines indirectly)
  ! on image 1 from the involved multiple remote images (2,3,4):
  ! (the receiving image (image 1) will execute this rotuine)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intTestArrayUpperBound
  integer(OOOGglob_kint), dimension (1:intNumberOfRemoteImages, 1:intTestArrayUpperBound), intent (out) :: &
                                                                               intA_TestArrayForRemoteTransfer
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intArrayElementSyncStat
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint), intent(in) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:intNumberOfRemoteImages), intent(in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint), dimension (1:intNumberOfRemoteImages, 1:intTestArrayUpperBound, 1:2) :: &
                                                             intA_ArrayElementSyncStatAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intCount2
  integer(OOOGglob_kint) :: intEnumStepWidth
  integer(OOOGglob_kint) :: intRemoteChannelNumberForLocalAtomicSet
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA")
  !
  !**********************************************************************
  ! (1) reset the TestArray component locally on this image (to allow for
  ! synchronization of the array elements later on):
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementNotSynchronizedYet
  ! - pack the Enum value together with the value 0 for the TestArray elements:
  intEnumStepWidth = OOOPimscEnum_ArrayElementSyncStat % Enum_StepWidth ! only for error checking
  call OOOPimsc_PackEnumValue (Object_CA, intArrayElementSyncStat, &
                                         0, intPackedEnumValue, intEnumStepWidth)
  ! - fill the local TestArray component atomicly:
  do intCount2 = 1, intNumberOfRemoteImages
    intRemoteChannelNumberForLocalAtomicSet = intA_RemoteImageNumbers(intCount2)
    do intCount = 1, ubound(intA_TestArrayForRemoteTransfer, 1)
      call OOOPimscSAElement_atomic_intTestArray_CA (Object_CA, intPackedEnumValue, &
                            this_image(), intArrayIndex = intCount, logExecuteSyncMemory = .false., &
                intRemoteChannelNumberForLocalAtomicSet = intRemoteChannelNumberForLocalAtomicSet)
                          ! here, we do not execute sync memory for local atomic_define yet
    end do
  end do
  !
  ! *********************************************************************
  ! (2) set this image to state 'WaitForTestArrayTransfer' and signal this to the remote images (2,3,4):
  ! (conterpart synchronization routine is step 1 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForTestArrayTransfer
  !
  ! - pack the ImageActivityFlag together with this_image():
  intEnumStepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth ! only for error checking
  call OOOPimsc_PackEnumValue (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue, &
                                        intEnumStepWidth)
  !
  ! - signal to the remote images (2,3,4) that this image is now in state 'WaitForTestArrayTransfer':
  do intCount = 1, intNumberOfRemoteImages
    intRemoteImageNumber = intA_RemoteImageNumbers(intCount)
    call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                           intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  end do
  !
  !*************************************************************************
  ! (3) wait until all the (local) array elements of the TestArray coarray component
  ! (set from the multiple remote images 2,3,4) are in state ArrayElementSynchronized
  ! (counterpart routine is step 3 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intArrayElementSyncStat = OOOPimscEnum_ArrayElementSyncStat % ArrayElementSynchronized
  ! - spin-wait loop bulk synchronization for the array data transfer from the multiple remote images:
  !
  call OOOPimscSpinWaitBulkArrayRang1Sync_atomic_intTestArray_CA (Object_CA, intArrayElementSyncStat, &
                  intNumberOfRemoteImages, intA_RemoteImageNumbers, intTestArrayUpperBound, &
                  intA_ArrayElementSyncStatAndItsAdditionalAtomicValue, logExecuteSyncMemory = .true.)
  !
  intA_TestArrayForRemoteTransfer(:,:) = &
            intA_ArrayElementSyncStatAndItsAdditionalAtomicValue(:,:,2) ! procedure output argument
  !
  !*************************************************************************
  ! (4) signal to the remote images (2,3,4) that this image is now in state 'TestArrayRemoteTransferDone'
  ! (conterpart synchronization routine is step 4 in OOOPimsc_SynchronizeAndDistributeTheTestArray_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % TestArrayRemoteTransferDone

  do intCount = 1, intNumberOfRemoteImages
    intRemoteImageNumber = intA_RemoteImageNumbers(intCount)
    call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  end do

  !*************************************************************************
  ! (5) finish execution on the executing image (not really required for this example):
!  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
!                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !*************************************************************************
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA
```
