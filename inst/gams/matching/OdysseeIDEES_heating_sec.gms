sum(refMap_OdysseeIDEES_heating_sec(refVar,typ,hs),
  sum((bs,vin,loc,inc)$vinExists(t,vin),
    v_stock("area",bs,hs,vin,reg,loc,typ,inc,t)
  )
)$sameas(ref,"OdysseeIDEES_heating_sec")