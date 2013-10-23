library(shiny)
## Assumes global variables: ranges,which.response
shinyUI(bootstrapPage(
                      headerPanel('Maximum Membership Parameter Estimation',
                                  singleton(
                                             tags$head(
                                                       tags$style(HTML("
.control-label {display:inline-block;}
.span8 {width:95%}

#more>div>div{display:inline-block;width:90%}
#more>div>div>.control-label{width:25%;text-align:right}
#more>div>div>.jslider{display:inline-block;width:70%}
")),
                                                       tags$link(rel = 'stylesheet',
                                                                 type = 'text/css',
                                                                 href = '/tableinput/tableinput.css'),
                                                       tags$script(src = '/tableinput/tableinput.js'),
## TODO: grouping mechanism
                                                       tags$script(HTML("
$( document ).ready(function(){
  $('#more input:checkbox').on('change',function(){$('#more_slider_'+this.value.replace(/\\./g,'\\\\.').replace(/\\$/g,'\\\\$')+'~.jslider').toggle($(this).attr('checked')=='checked')});

$('#more_toggle_all').on('click',function(evt){
 $('#more input:checkbox').attr('checked', ! $('#more input:checkbox').attr('checked'));
 $('#more input:checkbox').trigger('change');
});
                                                                    })"))
                                                       )#head
                                            )#singleton
                                  ),    #headerPanel
                      mainPanel(id="more",
                                {
                                  sliders <- lapply(1:nrow(ranges),function(i) {
                                    frac <-c(round((ranges$Modeled[i]-ranges$Min[i])/(ranges$Max[i]-ranges$Min[i])*15),15)
                                    ticks <- rep("|",frac[2]+1)
                                    ticks[frac[1]+1] <- ranges$Modeled[i]
                                    if(ranges$Variable[i] %in% which.response){
                                      check <- ""
                                    } else {
                                      check <- tags$input(type = "checkbox", name = "more",
                                                          id = paste("more", i, sep = ""), value = ranges$Variable[i])
                                      ## TODO: checkboxes or radio buttons for response to use
                                    }
                                    div(
                                        ## TODO: show modeled value and breakeven value on slider?
                                        ## TODO: select which vars to use and which to calculate
                                        ## TODO: update when ranges change - can't use updateSliderInput
                                        check,
                                        sliderInput(sprintf("more_slider_%s",ranges$Variable[i]),ranges$Variable[i],
                                                    min=ranges$Min[i],max=ranges$Max[i],
                                                    value=c(ranges$Min[i],ranges$Max[i]),
                                                    ticks=ticks
                                                    )
                                        )
                                  })
                                  div(span("Toggle all",id="more_toggle_all"),br(),
                                      sliders,
                                      id="more",class="control-group shiny-input-checkboxgroup")
                                },
                                ##uiOutput("more_sliders"),
                                actionButton("more_update","Update"),
                                ##textInput("more_status","",""),
                                tableOutput("more_results")
                                )             #mainPanel
                      ))                      #shinyUI
