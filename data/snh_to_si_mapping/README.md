This folder includes mapping between cms' skilled nursing facilities to activity locations in NSSAC synthetic population ver. 2.0 for Virginia, US.

## Header

**federal\_provider\_number** unique id for skilled nursing facility
**si\_loc\_id** unique id for activity location
**distance** distance between skilled nursing facility's geo location and activity location (in meter)
**rfld\_risks** river flooding risk score
**rfld\_riskr** river flooding risk rank
**cfld\_risks** coastal flooding risk score
**cfld\_riskr** coastal flooding risk rank

## Notes

1. Flooding risks were assigned based on skilled nursing vacility's location using 2022 FEMA national risk index.
   (see ../community/climate\_change\_risk)
2. The TIGER/Line tract polygons for 2022 FEMA national risks is 2017.
3. More information on mapping can be found in ../../documents/products/processes/commute\_vulnerability/algorithm.md
