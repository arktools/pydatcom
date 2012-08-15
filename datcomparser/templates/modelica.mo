{%- macro print_table1d(table,x,indent='      ') -%}
  { {%- for row in table -%}
      {{'\n'}}{{indent}}{ {{- x[loop.index0]}},{{row -}} }
    {%- if not loop.last %},{% endif -%}
  {%- endfor -%} }
{%- endmacro -%}

{%- macro print_table2d(table,x,y,indent='      ') -%}
    { {{-'\n'}}{{indent}}{0,
  {%- for xI in x -%}
    {{xI}}
    {%- if not loop.last %},{% endif %}
  {%- endfor -%} },
  {%- for row in table -%}
    {{'\n'}}{{indent}}{ 
    {%- for entry in row -%}
      {%- if loop.first -%}
        {{y[loop.index0]}},
      {%- endif -%}
      {{entry}}{% if not loop.last %},{% endif %}
    {%- endfor -%} } {%- if not loop.last %},{% endif %}
  {%- endfor -%} }
{%- endmacro -%}

package {{name}}

  constant OpenFDM.Aerodynamics.Datcom.Table datcomTables(    

    // lift
    CL_Basic = {{ print_table1d(CL_Basic,alpha) }},
    dCL_Flap  = {{ print_table1d(dCL_Flap,flap) }},
    dCL_Elevator  = {{ print_table1d(dCL_Elevator,elev) }},
    // TODO check dCL_PitchRate = {{ print_table1d(dCL_PitchRate,alpha) }},
    dCL_AlphaDot  = {{ print_table1d(dCL_AlphaDot,alpha) }},

    // drag
    CD_Basic  = {{ print_table1d(CD_Basic,alpha) }},
    dCD_Flap  = {{ print_table2d(dCD_Flap,alpha,flap) }},
    dCD_Elevator  = {{ print_table2d(dCD_Elevator,alpha,elev) }},

    // side force
{#
    dCY_Beta  = {{dCY_Beta}},
    dCY_RollRate  = {{dCY_RollRate}},

    // roll moment
    dCl_Aileron  = {{dCL_Aileron}},
    dCl_Beta  = {{dCL_Beta}},
    dCl_RollRate  = {{dCL_RollRate}},
    dCl_YawRate  = {{dCL_YawRate}},

    // pitch moment
    Cm_Basic = {{Cm_Basic}},
    dCm_Flap  = {{dCm_Flap}},
    dCm_Elevator  = {{dCm_Elevator}},
    dCm_PitchRate  = {{dCm_PitchRate}},
    dCm_AlphaDot  = {{dCm_AlphaDot}},

    // yaw moment
    dCn_Aileron  = {{dCn_Aileron}},
    dCn_Beta  = {{dCn_Beta}},
    dCn_RollRate  = {{dCn_RollRate}},
    dCn_YawRate  = {{dCn_YawRate}};

#}
end {{name}};

// vim:ts=2:sw=2:expandtab
