# ErrorInstance

***Work in progress.***

An ErrorInstance is an object representing an error, containing an error code, error message, whether the error is critical (should stop the program executing), and a user-defined trace of where the error has come from.

<a name="creating"></a>
## Creating error instances

An `ErrorInstance(code, message, isCritical, trace)` interface is provided so that ErrorInstances can easily be created. The interface returns the created error.

| Parameter declaration | Description | Default |
| :--- | :--- | :--- |
| `integer :: code` | The error code. | - |
| `character(len=*), optional :: message` | Default message for the error (which there is plenty of opportunity to override). | "" |
| `logical, optional :: isCritical` | Is the error critical? | .true. |
| `character(len=*), optional :: trace(:)` | Custom trace for the error |

<a name="traces"></a>
## Error traces