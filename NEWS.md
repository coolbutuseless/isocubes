
# isocubes 0.1.5.9001 

* [9000] Update to focus on SDF. Isocubes are used as an example renderer
* [9001] Moved SDF/CSG to another package

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
    * renamed `fill_left` and `fill_right` to `fill2` and `fill3` respectively

# isocubes 0.1.2  2022-06-23

* Replaced `max_y` argument with its inverse `ysize`
* Added Signed Distance Fields (`sdf_*()`) for 
    * object creation
    * object transformation
    * object combinations
  

# isocubes 0.1.1  2022-06-21

* `coords_heightmap()`
    * Corrected an accidental matrix transposition.
    * Added `flipx`, `flipy` and `ground` arguments for easier orientation 
      adjustments.
* `isocubesGrob` update:
    * Renamed args to `x` and `y`.
* Updated README examples

# isocubes 0.1.0  2022-06-20

* Initial release
