# MUT Code Standards

This document defines coding standards for the MUT (Modflow User Tool) Fortran codebase.

## General Principles

1. **Modern Fortran**: Use Fortran 2008/2018 features where beneficial while maintaining compatibility
2. **Type Safety**: Always use `implicit none` in all subroutines and functions
3. **Modularity**: Keep modules focused on a single responsibility
4. **Documentation**: Document public interfaces and complex algorithms
5. **Error Handling**: Use structured error handling instead of direct `stop` statements

## Naming Conventions

### Variables
- **Local variables**: Use descriptive names with lowercase and underscores (e.g., `node_count`, `max_iterations`)
- **Module-level variables**: Use descriptive names, consider prefixing with module name if needed
- **Constants**: Use UPPERCASE with underscores (e.g., `MAX_NODES`, `PI`)
- **Loop counters**: Use short names like `i`, `j`, `k` for simple loops; use descriptive names for complex loops

### Derived Types
- **Type names**: Use `t_` prefix (e.g., `t_point`, `t_mesh`, `t_line`)
- **Type components**: Use lowercase with underscores (e.g., `node_id`, `x_coordinate`)

### Subroutines and Functions
- **Names**: Use descriptive names with PascalCase or lowercase with underscores
- **Public routines**: Use clear, descriptive names
- **Private routines**: Can use shorter names if context is clear

### Modules
- **Module names**: Use PascalCase (e.g., `GeneralRoutines`, `ErrorHandling`)
- **File names**: Match module names (e.g., `GeneralRoutines.f90`)

### Arrays
- **Array variables**: Can use descriptive names (e.g., `node_array`, `element_list`)
- **Avoid**: Generic names like `array`, `data` without context

## Code Structure

### Module Organization
```fortran
module ModuleName
    use OtherModules
    implicit none
    private
    
    ! Public declarations
    public :: PublicType, PublicSubroutine
    
    ! Private declarations
    type :: PrivateType
        ! ...
    end type
    
    contains
    
    ! Public subroutines/functions
    subroutine PublicSubroutine(...)
        implicit none
        ! ...
    end subroutine
    
    ! Private subroutines/functions
    subroutine PrivateSubroutine(...)
        implicit none
        ! ...
    end subroutine
    
end module
```

### Subroutine/Function Structure
```fortran
subroutine SubroutineName(arg1, arg2)
    implicit none
    ! Intent declarations
    integer(i4), intent(in) :: arg1
    real(dp), intent(out) :: arg2
    
    ! Local variable declarations
    integer(i4) :: local_var
    real(dp) :: temp
    
    ! Implementation
    ! ...
end subroutine
```

## Type Declarations

### Always Use Kind Parameters
```fortran
use KindParameters, only: i4, dp, sp

integer(i4) :: count
real(dp) :: value
real(sp) :: single_precision_value
```

### Intent Attributes
- Always specify `intent` for all dummy arguments:
  - `intent(in)` for input-only arguments
  - `intent(out)` for output-only arguments
  - `intent(inout)` for arguments that are both input and output

## Error Handling

### Use ErrorHandling Module
```fortran
use ErrorHandling, only: ERR_SUCCESS, ERR_ALLOCATION, HandleError

integer(i4) :: ierr
allocate(array(size), stat=ierr)
if(ierr /= 0) then
    call HandleError(ERR_ALLOCATION, 'Failed to allocate array', 'SubroutineName')
end if
```

### Avoid Direct Stop Statements
- **Bad**: `stop 'Error message'`
- **Good**: `call HandleError(ERR_CODE, 'Error message', 'Context')`

## Control Flow

### Avoid GOTO Statements
- **Bad**: 
  ```fortran
  10 continue
      if(condition) goto 10
  ```
- **Good**:
  ```fortran
  do
      if(.not. condition) exit
      ! loop body
  end do
  ```

### Use Named Loops
```fortran
outer_loop: do i = 1, n
    inner_loop: do j = 1, m
        if(some_condition) cycle outer_loop
        ! ...
    end do inner_loop
end do outer_loop
```

## Documentation

### Inline Comments
- Use `!` for regular comments
- Use `!>` for documentation comments (if using documentation tools)
- Explain *why*, not *what* (the code should be self-explanatory)

### Module Header
```fortran
module ModuleName
    !### Brief description of module purpose
    ! 
    ! Detailed description of what this module does,
    ! its key features, and usage examples.
    !
    ! @author Author Name
    ! @date Date
    ! @version Version number
```

### Subroutine Header
```fortran
subroutine SubroutineName(arg1, arg2)
    ! Brief description of what the subroutine does
    !
    ! @param[in] arg1 Description of argument 1
    ! @param[out] arg2 Description of argument 2
    ! @return Description of return value (for functions)
```

## File Organization

### File Size Guidelines
- **Modules**: Aim for < 3000 lines per module
- **Subroutines**: Aim for < 200 lines per subroutine
- If a module/subroutine exceeds these limits, consider splitting it

### Compilation Order
Modules should be organized so that:
1. Base modules (e.g., `KindParameters`) have no dependencies
2. Utility modules depend only on base modules
3. Application modules depend on utility modules

## Best Practices

### Memory Management
- Always check allocation status: `allocate(array(size), stat=ierr)`
- Use `move_alloc` for efficient array growth
- Deallocate arrays when no longer needed

### Performance
- Use `intent` attributes to help compiler optimize
- Consider using `contiguous` attribute for arrays when appropriate
- Profile before optimizing

### Testing
- Test each module independently when possible
- Maintain regression test suite
- Test error conditions, not just happy paths

## Migration Notes

When refactoring existing code:
1. Add `implicit none` to all routines missing it
2. Replace `goto` statements with structured control flow
3. Replace direct `stop` with error handling routines
4. Add `intent` attributes to all dummy arguments
5. Split large modules/subroutines incrementally
6. Maintain backward compatibility during migration

## Version History

- **2025-01**: Initial version created as part of code quality improvement plan

