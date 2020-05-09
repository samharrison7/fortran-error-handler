---
title: A comprehensive Fortran error handling framework
tags:
    - Fortran
    - error handling
authors:
    - name: Sam Harrison
      orcid: 0000-0001-8491-4720 
      affiliation: 1
    - name: Virginie D Keller
      orcid:
      affiliation: 2
    - name: Richard J Williams
      orcid: 
      affiliation: 2
    - name: Michael Hutchins
      orcid:
      affiliation: 2
    - name: Stephen Lofts
      orcid:
      affilication: 1
affiliations:
    - name: UK Centre for Ecology & Hydrology, Lancaster Environment Centre, Library Avenue, Bailrigg, Lancaster, LA1 4AP, UK
      index: 1
    - name: UK Centre for Ecology & Hydrology, Maclean Building, Benson Lane, Crowmarsh Gifford, Wallingford, OX10 8BB, UK
      index: 2
date: 9 May 2020
bibliography: paper.bib
---

# Summary

Despite the rise of interpreted programming languages like Python and R in scientific programming, compiled languages are still the de-facto choice for computationally intensive modelling tasks, such as in climate sciences and theoretical physics. Fortran remains the top choice of many scientists, and Modern Fortran has brought great flexibility to the language in terms of object-oriented paradigms and polymorphism.

Any modelling code needs robust error checking, yet Fortran error handling frameworks to provide these utilities are few and far between, and those that do exist often implement only part of the error handling process, or rely on pre-processors [@popper:2012, @lucking:2015]. Here, we present what we believe is the most comprehensive Fortran error handling framework to date, providing a universal and comprehensive solution for applications requiring functional and robust error handling, utilising the power of modern object-oriented Fortran.

The framework implements the whole error handling process, including separate utilities for:
- Managing the error handling environment, through an `ErrorHandler` class. This allows for queuing and triggering error events, storing pre-defined custom errors and customising error reporting.
- Working with separate error instances, through an `ErrorInstance` class. Error instances include error metadata such as error code, message and whether the error is critical or just a warning. In addition, error instances (optionally) include a user-defined trace of where the error originated, enabling rapid debugging.
- Passing errors between routines, through a `Result` class. Result objects contain a list of error instances as well as a data component, enabling errors to be returned from functions at the same time as the data which the function would traditionally return.
- Common error checking criteria, through an `ErrorCriteria` class. This defines a number of common criteria used for error checking, such as equality and non-zero assertions.

These classes are extensible to enable, for example, custom error criteria by extending the `ErrorCriteria` class, or the passing of custom data types through extension of the `Result` object. They are designed to be modular, such that individual elements can be used separately to implement individual parts of the error handling process.The use of the whole framework enables robust error checking for the largest and most complex models and software.

# Acknowledgements

The Fortran Error Handler was developed within the European Union Horizon 2020 project [NanoFASE](http://nanofase.eu/), which received funding under grant agreement No 646002.

# References