# Computing device underclocking kit

This underclocking kit is one of a range of underclockers selected to
fit your hardware. It allows you to run the computation with lower speed
to preserve energy.

## Control 

In order to underclock your computer, run

    ship.actions.underclocker(frequency_divider)

Various computers have different default dividers. Note that higher
values of the divider correspond to lower speed. The underclocker will
refuse to overclock your computation device for safety reasons; it will
silently use the default divider instead of a smaller one.
