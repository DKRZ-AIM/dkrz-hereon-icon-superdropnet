! Fortran side

PROGRAM hello_world

  USE mpi

  USE mo_yac_finterface, ONLY: yac_finit, yac_fdef_comp, yac_fget_localcomm, &
       &                       yac_fdef_grid, yac_fdef_points,               &
       &                       yac_fset_global_index, yac_fset_core_mask,    &
       &                       yac_fset_mask, yac_fdef_field, yac_fsearch,   &
       &                       yac_ffinalize, yac_fput, yac_fget,            &
       &                       yac_fget_nbr_fields, yac_fget_field_ids,      &
       &                       yac_fget_model_timestep,                      &
       &                       yac_fget_coupling_timestep,                   &
       &                       yac_fget_role_of_field,                       &
       &                       yac_fget_field_name_from_id,                  &
       &                       yac_fget_field_id_from_name,                  &
       &                       yac_abort_message, YAC_LOCATION_CELL,         &
       &                       yac_fget_version


  IMPLICIT NONE

  CHARACTER(LEN=200) :: dummy_name, xml_filename, xsd_filename, &
    &                   comp_name, grid_name

  INTEGER :: id, role
  INTEGER :: i, info, ierror
  INTEGER :: comp_id

  INTEGER :: local_comm, npes, rank

  WRITE ( 6 , * ) "Hello world (Fortran side)"

  CALL mpi_init (ierror)

!  ! Initialise the coupler
!  xml_filename = "dummy_coupling.xml"
!  xsd_filename = "coupling.xsd"
!  CALL yac_finit ( xml_filename, xsd_filename )
!
!  ! Inform the coupler about what we are
!  comp_name = "hello_world"
!  grid_name = "hello_world_grid"
!  CALL yac_fdef_comp ( comp_name, comp_id )
!
!  print *, "YAC Version: ", TRIM(yac_fget_version())
!
!  CALL yac_fget_localcomm ( local_comm, comp_id )
!  print *, 'Local Comm', local_comm
!
!  CALL mpi_comm_rank ( local_comm, rank, ierror )
!  CALL mpi_comm_size ( local_comm, npes, ierror )
!
!  WRITE ( 6 , * ) TRIM(comp_name), " rank ", rank, ": local size is ", npes
!
!  CALL yac_ffinalize
!
!  CALL mpi_finalize (ierror)

END PROGRAM hello_world
