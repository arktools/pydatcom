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
    dCL_PitchRate = {{ print_table1d(dCL_PitchRate,alpha) }},
    dCL_AlphaDot  = {{ print_table1d(dCL_AlphaDot,alpha) }},

    // drag
    CD_Basic  = {{ print_table1d(CD_Basic,alpha) }},
    // TODO  correct 2d printing
    dCD_Flap  = {{ print_table2d(dCD_Flap,alpha,flap) }},
    dCD_Elevator  = {{ print_table2d(dCD_Elevator,alpha,elev) }},

    // side force
    dCY_Beta  = {{ print_table1d(dCY_Beta,alpha) }},
    dCY_RollRate  = {{ print_table1d(dCY_RollRate,alpha) }},

    // roll moment
    dCl_Aileron  = {{ print_table1d(dCl_Aileron,alrn) }},
    dCl_Beta  = {{ print_table1d(dCl_Beta,alpha) }},
    dCl_RollRate  = {{ print_table1d(dCl_RollRate,alpha) }},
    dCl_YawRate  = {{ print_table1d(dCl_YawRate,alpha) }},

    // pitch moment
    Cm_Basic = {{ print_table1d(Cm_Basic,alpha) }},
    dCm_Flap  = {{ print_table1d(dCm_Flap,flap) }},
    dCm_Elevator  = {{ print_table1d(dCm_Elevator,elev) }},
    dCm_PitchRate  = {{ print_table1d(dCm_PitchRate,alpha) }},
    dCm_AlphaDot  = {{ print_table1d(dCm_AlphaDot,alpha) }},

    // yaw moment
    dCn_Aileron  = {{ print_table2d(dCn_Aileron,alpha,alrn) }},
{#
    dCn_Beta  = {{dCn_Beta}},
    dCn_RollRate  = {{dCn_RollRate}},
    dCn_YawRate  = {{dCn_YawRate}};

#}
end {{name}};

// vim:ts=2:sw=2:expandtab
