#ui <- USERLogin()

ui <- shinyUI(tagList(ui_htmlHead,
                      dashboardPage(head=ui_header_OUT,
                                    sidebar=ui_sidebar_OUT,
                                    body=ui_body_OUT,
                                    skin=LOCATION_SKIN)))