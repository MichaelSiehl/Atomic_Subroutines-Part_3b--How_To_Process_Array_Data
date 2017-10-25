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
