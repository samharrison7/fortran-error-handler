project('feh', 'fortran', meson_version: '>=0.50')

# Compiler flags
flags = []
if meson.get_compiler('fortran').get_id() == 'gcc'
    flags = ['-fbackslash', '-fbacktrace']
endif
if meson.get_compiler('fortran').get_id() == 'intel'
    flags = ['-assume', 'bscc', '-traceback']
endif
add_project_arguments(flags, language : 'fortran')

# Files that are common to all build
files = ['src/ErrorInstance.f90', 'src/ErrorHandler.f90',
         'src/ErrorCriteria.f90', 'src/Result.f90']

# Add example executable
executable('example', files(files + ['example/example_usage.f90']))

# Add an exe for the test
test = executable('testexe', files(files + ['tests/assert.f90',
                                   'tests/util.f90',
                                   'tests/tests_ErrorCriteria.f90',
                                   'tests/tests_ErrorInstance.f90',
                                   'tests/tests_Result.f90',
                                   'tests/run_tests.f90']))
# Create the test itself
test('test', test)

# Add the library and make it installable
library('feh', files, install : true)