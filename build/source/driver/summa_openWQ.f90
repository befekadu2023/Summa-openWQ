module summa_openWQ
  USE nrtype
  USE openWQ, only:ClassWQ_OpenWQ
  USE data_types, only:gru_hru_doubleVec
  implicit none
  private
  ! Subroutines
  public :: init_openwq
  public :: run_time_start
  public :: run_time_start_go
  public :: run_space_step
  public :: run_time_end

  ! Global Data for prognostic Variables of HRUs
  type(gru_hru_doubleVec),save,public   :: progStruct_timestep_start ! copy of progStruct at the start of timestep for passing fluxes


  contains

  ! Subroutine to initalize the openWQ object
  ! putting it here to keep the SUMMA_Driver clean
subroutine init_openwq(err, message)
  USE globalData,only:openWQ_obj
  USE globalData,only:gru_struc                               ! gru-hru mapping structures
  USE globalData,only:prog_meta
  USE allocspace_module,only:allocGlobal                      ! module to allocate space for global data structures

  implicit none

  integer(i4b),intent(inout)                      :: err
  character(*),intent(inout)                      :: message         ! error messgage
  integer(i4b)                                    :: hruCount
  integer(i4b)                                    :: num_layers_canopy
  integer(i4b)                                    :: num_layers_matricHead
  integer(i4b)                                    :: num_layers_aquifer
  integer(i4b)                                    :: num_layers_volFracWat
  integer(i4b)                                    :: y_direction
  integer(i4b)                                    :: iGRU, iHRU          ! indices of GRUs and HRUs

  openwq_obj = ClassWQ_OpenWQ() ! initalize openWQ object

  ! nx -> num of HRUs)
  hruCount = sum( gru_struc(:)%hruCount )

  ! ny -> this seems to be fixes because SUMMA is based on the HRU concept, so grids are serialized)
  y_direction = 1

  ! Openwq nz (number of layers)
  num_layers_canopy = 1       ! Cannopy has only 1 layer
  num_layers_aquifer = 1      ! GW has only 1 layer
  num_layers_matricHead = 0   ! Soil may have multiple layers, and gru-hrus may have different values
  num_layers_volFracWat = 0   ! Soil has multiple layers, and gru-hrus may have different values
  do iGRU = 1, size(gru_struc(:))
    do iHRU = 1, gru_struc(iGRU)%hruCount
      num_layers_matricHead = max( gru_struc(iGRU)%hruInfo(iHRU)%nSoil, num_layers_matricHead )
      num_layers_volFracWat = max( gru_struc(iGRU)%hruInfo(iHRU)%nSoil, num_layers_volFracWat )
    enddo
  enddo

  err=openwq_obj%decl(hruCount, num_layers_canopy, num_layers_matricHead, num_layers_aquifer, num_layers_volFracWat, y_direction)  ! intialize openWQ
  
  ! Create copy of state information, needed for passing to openWQ with fluxes that require
  ! the previous time_steps volume
  call allocGlobal(prog_meta, progStruct_timestep_start, err, message) 

end subroutine init_openwq
  
! Subroutine that SUMMA calls to pass varialbes that need to go to
! openWQ - the copy of progStruct is done in here
subroutine run_time_start(openWQ_obj, summa1_struc)
  USE summa_type, only: summa1_type_dec            ! master summa data type
  USE globalData, only: gru_struc

  implicit none

  ! Dummy Varialbes
  class(ClassWQ_OpenWQ), intent(in)  :: openWQ_obj
  type(summa1_type_dec), intent(in)  :: summa1_struc
  ! local variables
  integer(i4b)                       :: iGRU
  integer(i4b)                       :: iHRU
  integer(i4b)                       :: maxNumLayers_soil ! maximum number of layers for soil
  integer(i4b)                       :: maxNumLayers_snow ! maximum number of layers for snow)
  integer(i4b)                       :: err

  ! Get number of soil and snow layers
  ! Needs to be isolated because explicit-shaped arrays can only be defined with parameters 
  ! or int passed as an argument
  maxNumLayers_soil = 0
  maxNumLayers_snow = 0
  do iGRU = 1, size(gru_struc(:))
    do iHRU = 1, gru_struc(iGRU)%hruCount
      maxNumLayers_snow = max( gru_struc(iGRU)%hruInfo(iHRU)%nSnow, maxNumLayers_snow )
      maxNumLayers_soil = max( gru_struc(iGRU)%hruInfo(iHRU)%nSoil, maxNumLayers_soil )
    enddo
  enddo

  call run_time_start_go(openwq_obj, summa1_struc, maxNumLayers_snow, maxNumLayers_soil)

end subroutine

subroutine run_time_start_go(openWQ_obj, summa1_struc, maxNumLayers_snow, maxNumLayers_soil)
  USE summa_type, only: summa1_type_dec            ! master summa data type
  USE globalData, only: gru_struc
  USE var_lookup, only: iLookPROG  ! named variables for state variables
  USE var_lookup, only: iLookTIME  ! named variables for time data structure
  USE var_lookup, only: iLookATTR  ! named variables for real valued attribute data structure

  implicit none

  ! Dummy Varialbes
  class(ClassWQ_OpenWQ), intent(in)   :: openWQ_obj
  type(summa1_type_dec), intent(in)   :: summa1_struc
  ! local variables
  integer(i4b), intent(in)            :: maxNumLayers_snow
  integer(i4b), intent(in)            :: maxNumLayers_soil
  integer(i4b)                        :: iGRU
  integer(i4b)                        :: iHRU
  integer(i4b)                        :: ilay
  integer(i4b)                        :: iVar
  integer(i4b)                        :: iDat
  integer(i4b)                        :: openWQArrayIndex
  integer(i4b)                        :: simtime(5) ! 5 time values yy-mm-dd-hh-min
  real(rkind)                         :: airTemp(sum(gru_struc(:)%hruCount))
  real(rkind)                         :: canopyWat_vol(sum(gru_struc(:)%hruCount))
  real(rkind)                         :: aquiferStorage_vol(sum(gru_struc(:)%hruCount))
  real(rkind)                         :: swe_vol(sum(gru_struc(:)%hruCount), maxNumLayers_snow)
  real(rkind)                         :: matricHead_vol(sum(gru_struc(:)%hruCount), maxNumLayers_soil)
  real(rkind)                         :: soilTemp(sum(gru_struc(:)%hruCount), maxNumLayers_soil)
  real(rkind)                         :: soilMoisture(sum(gru_struc(:)%hruCount), maxNumLayers_soil)
  integer(i4b)                        :: err

  summaVars: associate(&
      progStruct     => summa1_struc%progStruct             , &
      timeStruct     => summa1_struc%timeStruct             , &
      attrStruct     => summa1_struc%attrStruct               &
  )

  ! Update dependencies and storage volumes
  ! Assemble the data to send to openWQ
  openWQArrayIndex = 0 ! index into the arrays that are being passed to openWQ
  do iGRU = 1, size(gru_struc(:))
      do iHRU = 1, gru_struc(iGRU)%hruCount

        openWQArrayIndex = openWQArrayIndex + 1 

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Update layered variables

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Update scaler dependencies
        ! Tair (Summa in K) -> convert to degrees C for Openwq
        airTemp(openWQArrayIndex) = &
          progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarCanairTemp)%dat(1) &
          - 273.15

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Update scalar storage volumes
        ! Vegetation
        ! unit for volume = m3 (summa-to-openwq unit conversions needed)
        ! scalarCanopyWat [kg m-2], so needs to  to multiply by hru area [m2] and divide by water density
        canopyWat_vol(openWQArrayIndex) = &
          progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarCanopyWat)%dat(1) &
          * attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea) / 1000
        ! Aquifer
        ! unit for volume = m3 (summa-to-openwq unit conversions needed)
        ! scalarAquiferStorage [m], so needs to  to multiply by hru area [m2] only
        aquiferStorage_vol(openWQArrayIndex) = &
          progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarAquiferStorage)%dat(1) &
          * attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea)

        ! Update soil variables and dependenecies
        do ilay = 1, maxNumLayers_soil
          
          ! Tsoil
          ! (Summa in K) -> convert to degrees C for Openwq
          soilTemp(openWQArrayIndex, ilay) = &
            progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%mLayerTemp)%dat(ilay) &
            - 273.15

          soilMoisture(openWQArrayIndex, ilay) = 0     ! TODO: Find the value for this varaibles

          ! Soil
          ! unit for volume = m3 (summa-to-openwq unit conversions needed)
          ! mLayerMatricHead [m], so needs to  to multiply by hru area [m2]
          matricHead_vol(openWQArrayIndex, ilay) = &
            progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%mLayerMatricHead)%dat(ilay) &
            * attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea)
    
        enddo

        ! Update snow variables and dependenecies
        do ilay = 1, maxNumLayers_snow
          
          ! Snow
          ! unit for volume = m3 (summa-to-openwq unit conversions needed)
          ! scalarSWE [kg m-2], so needs to  to multiply by hru area [m2] and divide by water density
          swe_vol(openWQArrayIndex, ilay) = &
            progStruct%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarSWE)%dat(ilay) &
            * attrStruct%gru(iGRU)%hru(iHRU)%var(iLookATTR%HRUarea) &
            / 1000

        enddo

        ! **************  
        ! Fluxes
        !************** 

        ! TODO

        ! Copy the prog structure
        do iVar = 1, size(progStruct%gru(iGRU)%hru(iHRU)%var)
          do iDat = 1, size(progStruct%gru(iGRU)%hru(iHRU)%var(iVar)%dat)
            progStruct_timestep_start%gru(iGRU)%hru(iHRU)%var(iVar)%dat(iDat) = progStruct%gru(iGRU)%hru(iHRU)%var(iVar)%dat(iDat)
          end do
        end do

      end do
  end do

  ! add the time values to the array
  simtime(1) = timeStruct%var(iLookTIME%iyyy)  ! Year
  simtime(2) = timeStruct%var(iLookTIME%im)    ! month
  simtime(3) = timeStruct%var(iLookTIME%id)    ! hour
  simtime(4) = timeStruct%var(iLookTIME%ih)    ! day
  simtime(5) = timeStruct%var(iLookTIME%imin)  ! minute

  err=openWQ_obj%run_time_start(&
        sum(gru_struc(:)%hruCount),             & ! total HRUs
        simtime,                                &
        soilMoisture,                           &                    
        soilTemp,                               &
        airTemp,                                &
        swe_vol,                                &
        canopyWat_vol,                          &
        matricHead_vol,                         &
        aquiferStorage_vol)

  ! copy progStruct values to progStruct_timestep_start


  end associate summaVars
end subroutine


subroutine run_space_step(timeStruct, fluxStruct, nGRU)
  USE var_lookup,   only: iLookPROG  ! named variables for state variables
  USE var_lookup,   only: iLookTIME  ! named variables for time data structure
  USE var_lookup,   only: iLookFLUX  ! named varaibles for flux data
  USE globalData,   only: openWQ_obj
  USE data_types,   only: var_dlength,var_i
  USE globalData,   only: gru_struc
  implicit none

  type(var_i),             intent(in)    :: timeStruct 
  type(gru_hru_doubleVec), intent(in)    :: fluxStruct
  integer(i4b),            intent(in)    :: nGRU

  integer(i4b)                           :: hru_index ! needed because openWQ saves hrus as a single array
  integer(i4b)                           :: iHRU      ! variable needed for looping
  integer(i4b)                           :: iGRU      ! variable needed for looping

  integer(i4b)                           :: simtime(5) ! 5 time values yy-mm-dd-hh-min
  integer(i4b)                           :: err
  ! compartment indexes
  integer(i4b)                           :: scalarCanopyWat=0 ! SUMMA Side units: kg m-2
  integer(i4b)                           :: mLayerMatricHead=1 ! SUMMA Side units: m
  integer(i4b)                           :: scalarAquifer=2 ! SUMMA Side units: m
  integer(i4b)                           :: mLayerVolFracWat=3 ! SUMMA Side units: ????
  integer(i4b)                           :: iy_r
  integer(i4b)                           :: iz_r
  integer(i4b)                           :: iy_s
  integer(i4b)                           :: iz_s

  ! Fluxes leaving the canopy
  real(rkind)                            :: scalarCanopySnowUnloading ! kg m-2 s-1
  real(rkind)                            :: scalarCanopyLiqDrainage   ! kg m_2 s-1



  simtime(1) = timeStruct%var(iLookTIME%iyyy)  ! Year
  simtime(2) = timeStruct%var(iLookTIME%im)    ! month
  simtime(3) = timeStruct%var(iLookTIME%id)    ! hour
  simtime(4) = timeStruct%var(iLookTIME%ih)    ! day
  simtime(5) = timeStruct%var(iLookTIME%imin)  ! minute
  
  hru_index=1
  do iGRU=1,nGRU
    do iHRU=1,gru_struc(iGRU)%hruCount
      ! Canopy Fluxes
      scalarCanopySnowUnloading = fluxStruct%gru(iGRU)%hru(iHRU)%var(iLookFLUX%scalarCanopySnowUnloading)%dat(1)
      scalarCanopyLiqDrainage = fluxStruct%gru(iGRU)%hru(iHRU)%var(iLookFLUX%scalarCanopyLiqDrainage)%dat(1)
      
      iy_s = 1
      iz_s = 1
      iy_r = 1
      iz_r = 1
      err=openwq_obj%run_space(simtime, &
                           scalarCanopyWat, hru_index, iy_s, iz_s, &
                           mLayerVolFracWat, hru_index, iy_r, iz_r, &
                           scalarCanopySnowUnloading, &
                           progStruct_timestep_start%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarCanopyWat)%dat(1))
      
      err=openwq_obj%run_space(simtime, &
                           scalarCanopyWat, hru_index, iy_s, iz_s, &
                           mLayerVolFracWat, hru_index, iy_r, iz_r, &
                           scalarCanopyLiqDrainage, &
                           progStruct_timestep_start%gru(iGRU)%hru(iHRU)%var(iLookPROG%scalarCanopyWat)%dat(1))



      hru_index = hru_index + hru_index
    end do
  end do



end subroutine run_space_step


subroutine run_time_end(openWQ_obj, summa1_struc)
  USE summa_type, only:summa1_type_dec            ! master summa data type
  
  USE var_lookup, only: iLookTIME  ! named variables for time data structure

  implicit none

  ! Dummy Varialbes
  class(ClassWQ_OpenWQ), intent(in)  :: openWQ_obj
  type(summa1_type_dec), intent(in)  :: summa1_struc

  ! Local Variables
  integer(i4b)                       :: simtime(5) ! 5 time values yy-mm-dd-hh-min
  integer(i4b)                       :: err ! error control

  summaVars: associate(&
      timeStruct     => summa1_struc%timeStruct       &       
  )

  simtime(1) = timeStruct%var(iLookTIME%iyyy)  ! Year
  simtime(2) = timeStruct%var(iLookTIME%im)    ! month
  simtime(3) = timeStruct%var(iLookTIME%id)    ! hour
  simtime(4) = timeStruct%var(iLookTIME%ih)    ! day
  simtime(5) = timeStruct%var(iLookTIME%imin)  ! minute

  err=openwq_obj%run_time_end(simtime)           ! minute

  end associate summaVars
end subroutine



end module summa_openWQ