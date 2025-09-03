
# isocubes 0.1.5.9013  2025-08-03

* [9000] Update to focus on SDF. Isocubes are used as an example renderer
* [9001] Moved SDF/CSG to another package
* [9002] Allow different coordinate systems
* [9003] Faster rendering with viewports for offset
* [9004] add demo data. tidy.
* [9005] fix compatability issue with output from lofifonts
* [9006] Faster colour handling
* [9007] Visibility check now done in C. Preparing the way for per-face 
         visibility check
* [9008] Turn off default visibility check when creating heightmap
* [9009] Allow cube border color to be set to the same as face color
* [9010] Use 'colorfast' package for color conversion
* [9011] xyplane = 'flat' by default
* [9012] transforms: translate, rotate, align
* [9013] export visibility checks

# isocubes 0.1.5  2022-08-04

* New single-pass, deterministic visibility checking algorithm.  
    * Replaces the prior limited-depth, iterative technique.
    * 4x faster
    * Determines correct visibility for all cubes in scene


# isocubes 0.1.4  2022-07-30

* Wrote a cheap, but inaccurate colour darkening function `cheap_darken()`
    * Quite a bit faster than `colorspace::darken()`
    * Removed `colorspace` as a dependency
    
# isocubes 0.1.3  2022-07-10

* Added a `light` argument to specify lighting direction
    * renamed `fill_left` and `fill_right` to `fill_left` and `fill_right` respectively

# isocubes 0.1.2  2022-06-23

* Replaced `max_y` argument with its inverse `ysize`
* Added Signed Distance Fields (`sdf_*()`) for 
    * object creation
    * object transformation
    * object combinations
  

# isocubes 0.1.1  2022-06-21

* `calc_heightmap_coords()`
    * Corrected an accidental matrix transposition.
    * Added `flipx`, `flipy` and `ground` arguments for easier orientation 
      adjustments.
* `isocubesGrob` update:
    * Renamed args to `x` and `y`.
* Updated README examples

# isocubes 0.1.0  2022-06-20

* Initial release
