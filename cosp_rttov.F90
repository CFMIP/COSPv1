! (c) British Crown Copyright 2008, the Met Office.
! All rights reserved.
! $Revision: 23 $, $Date: 2011-03-31 07:41:37 -0600 (Thu, 31 Mar 2011) $
! $URL: http://cfmip-obs-sim.googlecode.com/svn/stable/v1.4.0/cosp_rttov.F90 $
! 
! Redistribution and use in source and binary forms, with or without modification, are permitted 
! provided that the following conditions are met:
! 
!     * Redistributions of source code must retain the above copyright notice, this list 
!       of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright notice, this list
!       of conditions and the following disclaimer in the documentation and/or other materials 
!       provided with the distribution.
!     * Neither the name of the Met Office nor the names of its contributors may be used 
!       to endorse or promote products derived from this software without specific prior written 
!       permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
! IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
! FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
! IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
! OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

!
! History:
! Aug 2008 - V. John - Initial version
! Jun 2010 - A. Bodas-Salcedo - Conversion to module and tidy up
! 
MODULE MOD_COSP_RTTOV
    USE RTTOV_CONST, only : errorstatus_fatal, errorstatus_warning, errorstatus_success
    USE RTTOV_TYPES, only : rttov_coef, profile_type, transmission_type,   &
                            radiance_type, rttov_coef_scatt_ir, rttov_optpar_ir
    USE PARKIND1, Only : jpim, jprb
    IMPLICIT NONE
#include "rttov_errorreport.interface"
#include "rttov_setup.interface"
#include "rttov_errorhandling.interface"
#include "rttov_direct.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_dealloc_coef.interface"
CONTAINS
    SUBROUTINE RTTOV_MULTPROF( &
        nch_in,     & ! number of channels 
        ichan_in,   & ! channel indices
        surfem_in,  & ! surface emissivity values 
        prf_num_in, & ! number of profiles to simulate
        nlevels_in, & ! number of pressure levels
        plat_in,    & ! platform number
        sat_in,     & ! satellite number
        sens_in,    & ! instrument number
        zenang_in,  & ! zenith angle 
        p_in,       & ! Pressure [hPa]
        t_in,       & ! Temperature [ K ]
        q_in,       & ! Specific humidity [ ppmv ]
        o3_in,      & ! Ozone VMR [ ppmv ]
        co2_in,     & ! CO2 VMR [ ppmv ] *This is a single value* 
        ch4_in,     & ! CH4 VMR [ ppmv ] *This is a single value*
        n2o_in,     & ! N2O VMR [ ppmv ] *This is a single value*
        co_in,      & ! CO VMR [ ppmv ]  *This is a single value*
        h_surf,     & ! Surface height [ m ]
        u_surf,     & ! U wind at 10 m  [ m/s ]
        v_surf,     & ! V wind at 10 m [ m/s ]
        t_skin,     & ! Skin Temperatre [ K ] 
        p_surf,     & ! Surface pressure 
        t_surf,     & ! 1.5 m Temperature [ K ]
        q_surf,     & ! 1.5 m specific humidity [ ppmv ]
        lsmask,     & ! land sea mask
        latitude,   & ! Latitude [ deg north ]
        tbs         & ! Brightness temperature [ K ] (OUTPUT)
        )
    
    !------ Input arguments. No RTTOV kinds should be used here -----------------
    integer  :: nch_in             ! number of channels to be computed
    integer  :: ichan_in(nch_in)   ! Indices of selected channels 
    Real     :: surfem_in(nch_in)  ! Surface emissivities for the channels
    Integer  :: prf_num_in
    Integer  :: nlevels_in
    Integer plat_in  ! Satellite platform
    Integer sat_in   ! Satellite number
    Integer sens_in  ! Satellite sensor
    Real     :: zenang_in          ! Satellite zenith angle
    Real p_in(prf_num_in, nlevels_in)  ! Pressure profiles  
    Real t_in(prf_num_in, nlevels_in)  ! Temperature profiles
    Real q_in(prf_num_in, nlevels_in)  ! Humidity profiles
    Real o3_in(prf_num_in, nlevels_in) ! Ozone profiles
    ! The following trace gases contain constant values
    Real co2_in ! Carbon dioxide 
    Real ch4_in ! methane 
    Real n2o_in ! n2o 
    Real co_in  ! carbon monoxide
    Real h_surf(prf_num_in)         ! Surface height
    Real u_surf(prf_num_in)         ! U component of surface wind
    Real v_surf(prf_num_in)         ! V component of surface wind
    Real t_skin(prf_num_in)         ! Surface skin temperature
    Real p_surf(prf_num_in)         ! Surface pressure
    Real t_surf(prf_num_in)         ! 1.5 m Temperature
    Real q_surf(prf_num_in)         ! 1.5 m Specific humidity
    Real lsmask(prf_num_in)         ! land-sea mask
    Real latitude(prf_num_in)       ! Latitude
    Real tbs(prf_num_in, nch_in)  ! Tbs (in the right format)
    
    !------ Local variables. Use only RTTOV kinds or derived types.
    !       logical variables are declared with the same kind
    !       as integers, as they are affected inthe same way by flags like -qintsize=8
    type( rttov_coef ), allocatable      :: coef(:)       ! coefficients
    type(profile_type), allocatable      :: profiles(:)
    type(transmission_type)              :: transmission
    type(radiance_type)                  :: radiance
    type(rttov_coef_scatt_ir), allocatable:: coef_scatt_ir(:)
    type(rttov_optpar_ir), allocatable    :: optp(:)
    
    Integer(Kind=jpim), Allocatable :: instrument(:,:) ! instrument id
    Integer(Kind=jpim), Allocatable :: nchan(:,:) ! number of channels per instrument and profile
    Integer(Kind=jpim), Allocatable :: nchan1(:) ! number of channels per instrument and profile
    Integer(Kind=jpim), Allocatable :: nchannels(:) ! number of channels per instrument 
    Integer(Kind=jpim), Allocatable :: ifull(:)   ! full test (with TL,AD,K) per instrument
    Integer(Kind=jpim), Allocatable :: nprof(:)   ! number of profiles per instrument
    Integer(Kind=jpim), Allocatable :: nsurf(:)   ! surface id number per instrument
    Integer(Kind=jpim), Allocatable :: nwater(:)  ! water id number per instrument
    Integer(Kind=jpim), Allocatable :: ichan(:,:)   ! channel list per instrument
    Integer(Kind=jpim), Allocatable :: channels(:)    ! channel list per instrument*profiles
    Integer(Kind=jpim), Allocatable :: lprofiles  (:)
    Integer(Kind=jpim) :: nprofiles, iref, isun, asw 
    Integer(Kind=jpim), Allocatable :: rttov_errorstatus(:)  ! rttov error return code
    Integer(Kind=jpim), Allocatable :: setup_errorstatus(:)  ! setup return code
    Integer(Kind=jpim) :: mxchn 
    Integer(Kind=jpim) :: Err_Unit        ! Logical error unit (<0 for default)
    Integer(Kind=jpim) :: verbosity_level ! (<0 for default)
    Integer(Kind=jpim) :: nrttovid     ! maximum number of instruments
    Integer(Kind=jpim) :: no_id        ! instrument loop index
    Integer(Kind=jpim) :: i, j, jch
    Integer(Kind=jpim) :: Nprofs  ! Number of calls to RTTOV
    Integer(Kind=jpim) :: nch ! intermediate variable
    Integer(Kind=jpim) :: errorstatus, io_status
    Integer(Kind=jpim) :: ioout, interp
    Integer(Kind=jpim), Parameter :: jpnav  =  31    ! no. of profile variables
    Integer(Kind=jpim), Parameter :: jpnsav =  6       ! no. of surface air variables
    Integer(Kind=jpim), Parameter :: jpnssv =  6       ! no. of skin variables
    Integer(Kind=jpim), Parameter :: jpncv  =  2       ! no. of cloud variables
    Integer(Kind=jpim), Parameter :: sscvar = jpnsav+jpnssv+jpncv ! no of surface,skin,cloud vars
    Integer(Kind=jpim) :: alloc_status(60)
    
    Real(Kind=jprb),    Allocatable :: surfem(:,:)   ! surface input emissivity per channel , instrument
    Real(Kind=jprb),    Allocatable :: emissivity (:)
    Real(Kind=jprb),    Allocatable :: fresnrefl  (:)
    Real(Kind=jprb),    Allocatable :: input_emissivity (:)
    Real(Kind=jprb) :: tbs_temp(nch_in * prf_num_in) ! A temporary variable to hold Tbs
    Real(Kind=jprb) :: zenang, azang, sunzang, sunazang

    Character (len=3)  :: cref
    Character (len=3)  :: csun
    Character (len=80) :: errMessage
    Character (len=14)  :: NameOfRoutine = 'rttov_multprof'
    
    Logical :: addinterp  ! switch for the interpolator
    Logical,Allocatable :: calcemis  (:)
    logical :: refrac, solrad, laerosl, lclouds, lsun, all_channels

    ! Local variables for input arguments that need type casting to avoid type-mismatch with 
    ! RTTOV kinds. This happens with some compiler flags (-qintsize=8).
    Integer(Kind=jpim)  :: prof_num
    Integer(Kind=jpim)  :: nlevels
    
    ! Type-casting of input arguments that need to be passed to RTTOV
    prof_num = prf_num_in
    nlevels  = nlevels_in
                
    ! Unit numbers for input/output
    IOOUT = 2
    !- End of header --------------------------------------------------------
    
    ! Curretly we plan to calculate only 1 instrument per call
    nrttovid  =  1
    
    mxchn  =  nch_in
    
    errorstatus     = 0
    alloc_status(:) = 0
    all_channels = .false.
    sunzang = 0._jprb
    sunazang = 0._jprb
    !
    !Initialise error management with default value for
    ! the error unit number and
    ! Fatal error message output
    Err_unit = -1
    verbosity_level = 0
    ! All error message output
!     verbosity_level = 3
    call rttov_errorhandling( Err_unit, verbosity_level, print_checkinput_warnings=.false. )
    
    io_status = 0
    errmessage = ''
    
    ! Assigning the zenith angle
    zenang = zenang_in
    
    ! Beginning of Routine.
    ! ---------------------
    
    allocate (coef(nrttovid),stat= alloc_status(1))
    allocate (coef_scatt_ir(nrttovid),stat= alloc_status(2))
    allocate (optp(nrttovid),stat= alloc_status(3))
    
    allocate (instrument(3,nrttovid),stat= alloc_status(4))
    allocate (ifull(nrttovid),stat= alloc_status(5))
    allocate (nprof(nrttovid),stat= alloc_status(6))
    allocate (nsurf(nrttovid),stat= alloc_status(7))
    allocate (nwater(nrttovid),stat= alloc_status(8))
    allocate (nchannels(nrttovid),stat= alloc_status(9))
    allocate (nchan1(nrttovid),stat= alloc_status(10))
    !maximum number of channels allowed for one instrument is mxchn
    allocate (surfem(mxchn,nrttovid),stat= alloc_status(11))
    allocate (ichan (mxchn,nrttovid),stat= alloc_status(12))
    If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
    End If
    
    surfem(:,:)  =  0.0_JPRB
    ichan(:,:)   =  0
    
    !!! FIXME: Shall we get rid of this loop? We use only one instrument
    DO NO_ID = 1, NRTTOVID
        
        instrument(1,no_id) = plat_in
        instrument(2,no_id) = sat_in
        instrument(3,no_id) = sens_in
                
        !! Forward model only (0) or TL and AD (1) or K (2)?'
        !  This version supports only Forward model
        IFULL(no_id) = 0
        
        !! ' Number of profiles to test per call? '
        NPROF(no_id)  = prof_num
        nprofiles = NPROF(no_id)
        
        !! ' Total number of profiles to process? '
        NPROFS  = prof_num
        NPROFS = NPROFS / NPROF(no_id)   ! Number of calls to RTTOV
        
        !FIXME: I am directly using a land -sea mask. 
        !FIXME: Check whether it is 0 for land and 1 for sea in the mask. 
        !FIXME: Otherwise provide a reverse mask
        !
        !PRINT  *, ' Surface type (0=land, 1=sea, 2=ice/snow)? '
        !NSURF(no_id)  = 0
    
    
        !PRINT  *, ' Water type (0=fresh water, 1=ocean water)'
        !! FIXME: Check whether it is OK to use ocean all the time
        !
        NWATER(no_id) = 1
    
        !..SET UP CHANNEL NUMBERS
        allocate (nchan(nprof(no_id),nrttovid),stat= alloc_status(3))
        nchan(1:nprof(no_id),no_id) = nch_in
            
        ichan(:, 1)   =  ichan_in
        surfem(:, 1)  =  surfem_in
    
        ! nchan(1,no_id) is now the real number of channels selected
        do j = 1 , nprof(no_id)
            nchan(j,no_id) = nch_in  
        enddo
        
        ! compute channels*profiles
        nchannels(no_id) = 0
        Do j = 1 , nprof(no_id)
            nchannels(no_id) = nchannels(no_id) + nchan (j,no_id)
        End Do
        nchan1(no_id) = nchan(1,no_id)
        !
    END DO
    
    !write(6,*)'Do you want to include aerosols?' No!!
    !iaer    = 0
    laerosl = .False.
    
    !write(6,*)'Do you want to include clouds?' No!!
    !icld    = 0
    lclouds = .False.
    
    !---------------------------------------------------------
    ! Beginning of rttov_setup test
    !---------------------------------------------------------
    alloc_status = 0
    allocate ( setup_errorstatus(nrttovid),stat= alloc_status(1))
    If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error for errorsetup")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
    End If
    
    If (all_channels)Then
        Call rttov_setup (&
            & setup_errorstatus,  &! out
            & Err_unit,           &! in
            & verbosity_level,    &! in
            & nrttovid,           &! in
            & laerosl,            &! in
            & lclouds,            &! in
            & coef,               &! out
            & coef_scatt_ir,      &! out
            & optp,               &
            & instrument)         ! in
    Else
        Call rttov_setup (&
            & setup_errorstatus,  &! out
            & Err_unit,           &! in
            & verbosity_level,    &! in
            & nrttovid,           &! in
            & laerosl,            &! in
            & lclouds,            &! in
            & coef,               &! out
            & coef_scatt_ir,      &! out
            & optp,               &
            & instrument,         &! in
            & ichan              ) ! in Optional 
    Endif
    if(any(setup_errorstatus(:) /= errorstatus_success ) ) then
        print*, 'rttov_setup fatal error'
        stop
    endif
    
    deallocate( setup_errorstatus ,stat=alloc_status(1))
    If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem deallocation error for setup_errorstatus")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
    End If
    
    DO no_id = 1, NRTTOVID
        if( any(coef(no_id)%ff_val_chn( : ) /= 1 )) then
        WRITE(*,*) ' some requested channels have bad validity parameter'
        do i = 1, nchan1(no_id)
            write(*,*) i, coef(no_id)%ff_val_chn(i)
            end do
        endif
    End Do
    !---------------------------------------------------------
    ! End of rttov_setup test
    !---------------------------------------------------------
    !
    
    DO no_id = 1, NRTTOVID
        
    !-----------------------------------
        ! Memory allocation for RTTOV_Direct
        !-----------------------------------
        allocate( rttov_errorstatus(nprof(no_id)),stat= alloc_status(3))
        ! Allocate profiles
        allocate( profiles(nprof(no_id)),stat= alloc_status(1))
        
        ! Allow profile interpolation
        interp  =  1
        
        if(interp == 0) addinterp = .false.
        if(interp == 1) addinterp = .true.
        asw = 1 ! allocate
        
        !if( no_id .gt. 1 ) stop
        
        call rttov_alloc_prof ( &
            &  errorstatus,          &
            &  nprof(no_id),         &
            &  profiles,             &
            &  nlevels,              &
            &  coef_scatt_ir(no_id), &
            &  asw,                  &
            &  addclouds = lclouds,  &
            &  addaerosl = laerosl,  &
            &  init = .true.         )
        
    
        !profiles(1) % nlevels = nlevels
        
        Do j = 1 , nprof(no_id)
            profiles(j) % nlevels =  nlevels
        Enddo
        
        alloc_status = 0_jpim
        ! number of channels per RTTOV call is only nchannels
        allocate( lprofiles  ( nchannels(no_id) )       ,stat= alloc_status(9))
        allocate( channels   ( nchannels(no_id) )        ,stat= alloc_status(10))
        allocate( emissivity ( nchannels(no_id) )       ,stat= alloc_status(12))
        allocate( fresnrefl  ( nchannels(no_id) )       ,stat= alloc_status(13))
        allocate( input_emissivity ( nchannels(no_id) ) ,stat= alloc_status(14))
        allocate( calcemis   ( nchannels(no_id) )        ,stat= alloc_status(15))
    
        ! allocate transmittance arrays with number of channels
        allocate( transmission % tau_layers (profiles(1) % nlevels,nchannels(no_id) ), &
            stat= alloc_status(11))
        allocate( transmission % tau_total (nchannels(no_id) )                       , &
            stat= alloc_status(12))
        
        If( Any(alloc_status /= 0) ) Then
            errorstatus = errorstatus_fatal
            Write( errMessage, '( "allocation of transmission")' )
            Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
            !IF (LHOOK) CALL DR_HOOK('RTTOV_DIRECT',1,ZHOOK_HANDLE)
            Stop
        End If
        
        transmission % tau_layers = 0._jprb
        transmission % tau_total = 0._jprb
    
        ! allocate radiance results arrays with number of channels
        asw = 1 ! allocate
        call rttov_alloc_rad (errorstatus,nchannels(no_id),radiance, &
            profiles(1)%nlevels,asw)
        
        AZANG  =  0
        ISUN   =  0
        IREF   =  1
    
        if(iref==0)then
            cref='NO'
            refrac=.False.
        else if(iref==1)then
            cref='YES'
            refrac=.True.
        endif
    
        if(sunzang<=87._JPRB)then
            solrad=.True.
        else
            solrad=.False.
        endif
        
        if(isun==1)then
            lsun=.true.
            if(sunzang<=87._JPRB)then
            csun='YES'
            solrad=.True.
            else
            csun='NO'
            solrad=.False.
            endif
        else
            csun='NO'
            solrad=.False.
        endif
        
        do i = 1, NPROF(no_id)
            profiles(i) %  p(:)  =  p_in(i, :)
            
            profiles(i) %  t(:)  =  t_in(i, :)
            profiles(i) %  q(:)  =  q_in(i, :)
            profiles(i) % o3(:)  =  o3_in(i, :)
            profiles(i) % co2(:) =  co2_in
            profiles(i) % ch4(:) =  ch4_in
            profiles(i) % n2o(:) =  n2o_in
            profiles(i) % co(:)  =  co_in
            
            profiles(i) % ozone_Data  =  .False.
            profiles(i) % co2_Data    =  .True.
            profiles(i) % n2o_data    =  .True.
            profiles(i) % ch4_Data    =  .True.
            profiles(i) % co_Data     =  .True.
            
            !FIXME: Make Cloud variables as passing ones if we go for all sky
            profiles(i) % cfraction  =  0.
            profiles(i) % ctp        =  500.
            
            profiles(i) % clw_Data   =  .False.
    
            ! 2m parameters 
            profiles(i) % s2m % p  =  p_surf(i)
            profiles(i) % s2m % t  =  t_in(i, 1)!t_surf(i)
            profiles(i) % s2m % q  =  q_in(i, 1)!q_surf(i)
            profiles(i) % s2m % u  =  2!u_surf(i)
            profiles(i) % s2m % v  =  2!v_surf(i)
        
            ! Skin variables for emissivity calculations
            profiles(i) % skin % t          =  t_skin(i)
            profiles(i) % skin % fastem(1)  =  3.0
            profiles(i) % skin % fastem(2)  =  5.0
            profiles(i) % skin % fastem(3)  =  15.0
            profiles(i) % skin % fastem(4)  =  0.1
            profiles(i) % skin % fastem(5)  =  0.3
            
            profiles(i) % zenangle      = zenang
            profiles(i) % azangle       = azang
            profiles(i) % latitude      = latitude(i)
            profiles(i) % elevation     = h_surf(i)
            profiles(i) % sunzenangle   = SUNZANG
            profiles(i) % sunazangle    = SUNAZANG
            profiles(i) % addsolar      = solrad
            profiles(i) % addrefrac     = refrac
            ! surface type
            profiles(i) % skin % surftype  = lsmask(i)
            !! FIXME: Check this one
            profiles(i) % skin % watertype = nwater(no_id)
            
            profiles(i) % aer_data   = laerosl
            profiles(i) % cld_data   = lclouds
            profiles(i) %idg         = 0._jprb
            profiles(i) %ish         = 0._jprb
            if( lclouds ) then
            profiles(i) %cloud(:,:)  = 0._jprb
            profiles(i) %cfrac(:,:)  = 0._jprb
            endif
        enddo
    
        ! Build the list of channels/profiles indices
        emissivity(:) = 0.0_JPRB
        channels(:) = 0_jpim
        lprofiles(:) = 0_jpim
        nch = 0_jpim
        Do j = 1 , nprof(no_id)
            DO  jch = 1,nchan1(no_id)
            nch = nch +1_jpim
            lprofiles ( nch ) = j
            if (all_channels)then
                channels( nch ) = ichan(jch,no_id)
            else 
                channels( nch ) = jch
            endif
            emissivity( nch ) = surfem(jch,no_id)
        End Do
        End Do
        
        input_emissivity(:) = emissivity(:)
        calcemis(:) = emissivity(:) < 0.01_JPRB
    
    
        ! FIXME: Check this one with Roger
        do j = 1 , NPROFS
        call rttov_direct(           &
                & rttov_errorstatus,    &! out
                & nprof(no_id),         &! in
                & nchannels(no_id),     &! in
                & channels,             &! in
                & lprofiles,            &! in
                & addinterp,            &! in
                & profiles,             &! in
                & coef(no_id),          &! in
                & coef_scatt_ir(no_id), &
                & optp(no_id)         , &
                & lsun,                 &! in
                & laerosl,              &! in
                & lclouds,              &! in
                & calcemis,             &! in
                & emissivity,           &! inout
                & transmission,         &! out
                & radiance  )            ! inout 
        enddo
    
        ! Initialising tbs array
        tbs(:, :)    =  0.0
        tbs_temp(:)  =  0.0
        tbs_temp     =  radiance%bt
    
        do i = 1, prof_num
        tbs(i, :)  =  tbs_temp((i-1)*nch_in+1:i*nch_in)
        enddo
        
    
        If ( any( rttov_errorstatus(:) == errorstatus_warning ) ) Then
        Do j = 1, nprof(no_id)
            If ( rttov_errorstatus(j) == errorstatus_warning ) Then
                write ( ioout, * ) 'rttov warning for profile', j
            End If
        End Do
        End If
        
        If ( any( rttov_errorstatus(:) == errorstatus_fatal ) ) Then
        Do j = 1, nprof(no_id)
            If ( rttov_errorstatus(j) == errorstatus_fatal ) Then
                write ( ioout, * ) 'rttov error for profile',j
            End If
        End Do
        Stop
        End If
    
        ! Deallocate
        ! number of channels per RTTOV call is only nchannels
        deallocate( channels   ,stat=alloc_status(2))
        deallocate( lprofiles  ,stat=alloc_status(3))
        deallocate( emissivity ,stat=alloc_status(4))
        deallocate( fresnrefl  ,stat=alloc_status(5))
        deallocate( calcemis   ,stat=alloc_status(6))
        deallocate( input_emissivity ,stat= alloc_status(14))
        If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem deallocation error for channels etc")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
        End If
        
        asw = 0 ! deallocate radiance arrays
        call rttov_alloc_rad (errorstatus,nchan1(no_id),radiance,profiles(1) % nlevels,asw)
        If(errorstatus /= errorstatus_success) Then
        Write( errMessage, '( "deallocation error for radiances")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Endif
        
        ! deallocate transmittances
        Deallocate( transmission % tau_total   ,stat= alloc_status(7))
        Deallocate( transmission % tau_layers  ,stat= alloc_status(8))
        If(errorstatus /= errorstatus_success) Then
        Write( errMessage, '( "deallocation error for transmittances")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
        Endif
        
        asw = 0 ! deallocate profile arrays
        call rttov_alloc_prof (errorstatus,nprof(no_id),profiles,profiles(1)%nlevels,coef_scatt_ir(no_id),asw,&
            & addclouds = lclouds, addaerosl = laerosl )
        deallocate( profiles,stat=alloc_status(1))
        If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem deallocation error for profiles")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Stop
        End If
    
    EndDo
    
    DO no_id = 1, NRTTOVID 
        Call rttov_dealloc_coef (errorstatus, coef(no_id),coef_scatt_ir(no_id),optp(no_id))
        If(errorstatus /= errorstatus_success) Then
        Write( errMessage, '( "deallocation error for coeffs")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Endif
    EndDo
    
    END SUBROUTINE RTTOV_MULTPROF
END MODULE MOD_COSP_RTTOV
