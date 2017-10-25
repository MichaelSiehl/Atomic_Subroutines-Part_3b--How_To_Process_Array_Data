! https://github.com/MichaelSiehl/Atomic_Subroutines--Part_3-b--How_To_Process_Array_Data

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
