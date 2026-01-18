# TODO

## Home Assistant feature parity

- reading sensors
  - temperature types
  - occupancy types
- triggers
  - clock time
  - been in state n for x duration
- derived sensors
  - don't use HASS implementations
  - reimplement in hasskell
  - e.g.
    - statistics
    - helpers
    - bayesian
- setting light
  - color temperature
  - brightness

## QoL

- check device connection type, and if they're wifi and all wifi devices
  get disconnected, then we can set a variable "wifi down" so automations
  that depend on wifi entities know not to trigger until wifi is back up again
  - this can either be a library or implemented at the event-system level

## Verification

- check for cyclic specifications
  - i.e. "if light a is on, turn light a off"
  - either dynamically (tracking last n world states and what they transition to)
  - or statically (determine syntactic cyclic dependencies using flow analysis)
