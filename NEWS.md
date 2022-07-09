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
    * Renamed args to `xo` and `yo`.
* Updated README examples

# isocubes 0.1.0  2022-06-20

* Initial release
