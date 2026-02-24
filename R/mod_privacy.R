#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_privacy_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML("
        .content {
          font-family: 'Roboto', sans-serif;
          color: #333;
          line-height: 1.6;
          padding: 20px;
        }
        .content h2 {
          color: #37a635;
          margin-top: 20px;
        }
        .content h5 {
          color: #37a635;
          margin-top: 15px;
        }
        .content p {
          text-align: justify;
          margin-bottom: 10px;
        }
        .content p.gray {
          color: gray;
        }
        .img-container {
          width: 100%;
          height: 150px;
          object-fit: cover;
          display: block;
          padding: 10px;
          margin-top: 20px;
          border-radius: 10px;
        }
      "))
    ),
    tags$img(src = "www/logos/casey-horner-wKjIeK4QSnk-unsplash.jpg",
             style = "width: 100%; height: 150px; object-fit: cover; display: block; padding: 10px; margin-top: 20px; border-radius: 10px;") ,
    fluidRow(
      column(10,
             offset = 1,
             div(class = "content",
                 h2("Terms"),
                                         p("The uPcycle Global Lakes Explorer is managed by the UK Centre for Ecology & Hydrology (UKCEH). The UKCEH is a not-for-profit company limited by guarantee with charitable status. UKCEH are referred to as 'We' below and in entering our Site you as a user ('You') are accepting our terms and conditions outlined below.",  style = "color:gray; text-align:justify;"),
                                         p("The UKCEH trademarks and logos ('the Trademarks') used and displayed on this Site are registered trademarks of UKCEH in the UK and other countries and may not be used without the prior written consent of the Trademark owner.", style = "color:gray; text-align:justify;"),
                                         h5("Using the uPcycle Global Lakes Explorer"),
                                         p("The uPcycle Global Lakes Explorer is maintained for your personal use and viewing. The access and use of the website featuring these terms and conditions constitutes your acceptance of these terms and conditions. They take effect from the date on which you first use this website.", style = "color:gray; text-align:justify;"),
                                         p("You agree to use this site only for lawful purposes, and in a manner that does not infringe the rights of, or restrict or inhibit the use and enjoyment of this site by any third party. Such restriction or inhibition includes, without limitation, conduct which is unlawful, or which may harass or cause distress or inconvenience to any person, and the transmission of obscene or offensive content or disruption of normal flow of dialogue within this site. Information collected by us is used for internal review to improve the content of the uPcycle Global Lakes Explorer. Information collected is not shared with other organisations except where stated. See our Cookies policy below for more details. For uses of data not related to website functionality, please read our Privacy Notice.", style = "color:gray; text-align:justify;"),
                                         p("For uses of data not related to website functionality, please read our Privacy Notice.", style = "color:gray; text-align:justify;"),
                                         h5("Linking to the uPcycle Global Lakes Explorer"),
                                         p("UKCEH welcomes and encourages other websites to link to the information that is hosted on these pages. You do not have to ask permission to link, but wording and linking graphics can be requested if necessary.", style = "color:gray; text-align:justify;"),
                                         h5("Links from this site"),
                                         p("Links from this site are not endorsed by UKCEH. Links and frames connecting this site with other sites are for convenience only and do not mean that UKCEH endorses or approves those other sites, their content or the people who run them. It is the responsibility of the Internet user to make their own decisions about the accuracy, currency, reliability and correctness of information found at sites linked from this website.", style = "color:gray; text-align:justify;"),
                                         h5("Virus protection"),
                                         p("We make every effort to check and test material at all stages of production. It is always wise for you to run an anti-virus program on all material downloaded from the Internet. We cannot accept any responsibility for any loss, disruption or damage to your data or your computer system which may occur whilst using material derived from this website.", style = "color:gray; text-align:justify;"),
                                         p("Information at this site: 1) is subject to the usual uncertainties of research, 2) is subject to change without notice, 3) should never be relied on as the basis for doing or failing to do something.", style = "color:gray; text-align:justify;"),
                                         h5("Disclaimer"),
                                         p("All information provided on the uPcycle Global Lakes Explorer is made available to provide immediate access for the convenience of interested persons. While UKCEH believes the information to be reliable, human or mechanical error remains a possibility. Therefore, UKCEH does not guarantee the accuracy, completeness, timeliness, or correct sequencing of the information. UKCEH, nor any of the sources of the information shall be responsible for any errors or omissions, or for the use of or results obtained from the use of this information.", style = "color:gray; text-align:justify;"),
                                         h5("Copyright statement"),
                                         p("By visiting the uPcycle Global Lakes Explorer and using the information available, you agree to the following: Copyright of the written material available at this site is owned by UKCEH (all images/photographs used on the website are also used in the report, their copyright is detailed in the chapter PDFs). All rights reserved. Visitors to the site may use information in the ways described in this legal notice:
	• you may download or print the uPcycle reports and materials provided on the website for research or personal use;
	• you must not change any of the material or remove any part of any copyright notice;
	• no information contained on the site may be used for commercial purposes, unless UKCEH has given its prior written permission.", style = "color:gray; text-align:justify;"),
                                         h5("Further information"),
                                         p("If you have a request or an enquiry about reproduction and rights, please write to enquiries@ceh.ac.uk.", style = "color:gray; text-align:justify;"),
                                         h5("Cookies policy"),
                                         p("When we provide web services, we want to make them easy, useful and reliable. This sometimes involves placing small amounts of information on your device, for example, computer or mobile phone. These include small files known as cookies. They cannot be used to identify you personally. These pieces of information are used to improve services for you through, for example, enabling a service to recognise your device so you don't have to give the same information several times during one task; recognising that you may already have given a username and password so you don't need to do it for every web page requested; measuring how many people are using services, so they can be made easier to use and there's enough capacity to ensure they are fast. Information collected is not shared with other organisations. Your web browser has settings which allow you to manage these small files yourself by removing or blocking cookies. Learn more about them through advice on the AboutCookies website.", style = "color:gray; text-align:justify;")


             )
      )
    ),

    # photo between sections
    tags$img(src = "www/logos/casey-horner-crop.jpg",
             style = "width: 100%; height: 150px; object-fit: cover; display: block; padding: 10px; margin-top: 20px; border-radius: 10px;"),

    fluidRow(
      column(10,
             offset = 1,
             div(class = "content",

             h2("Privacy"),
             p("The General Data Protection Regulation (GDPR) is a Europe-wide law that is part of the wider package of reform to data protection that includes the UK Data Protection Act (DPA) 2018. The GDPR and DPA 2018 set out requirements for how organisations will need to process personal data from 25 May 2018.", style = "color:gray; text-align:justify;"),
             p("This website is operated by the UK Centre for Ecology & Hydrology (UKCEH). This privacy notice tells you what to expect when your personal information is collected. It will be revised as required and you are encouraged to revisit the privacy notice regularly to read the latest version. This version is dated 1st December 2019.", style = "color:gray; text-align:justify;"),
             p("Please read the following carefully to understand our views and practices regarding your personal data and how we will process it. By visiting www.UKCEH.ac.uk and affiliated websites you are accepting and consenting to the practices described in this policy.", style = "color:gray; text-align:justify;"),
             h5("Who we are"),
             p("UK Centre for Ecology & Hydrology", style = "color:gray; text-align:justify;"),
             p("Maclean Building, Benson Lane", style = "color:gray; text-align:justify;"),
             p("Crowmarsh Gifford", style = "color:gray; text-align:justify;"),
             p("Wallingford", style = "color:gray; text-align:justify;"),
             p("Oxfordshire", style = "color:gray; text-align:justify;"),
             p("OX10 8BB", style = "color:gray; text-align:justify;"),
             p("T: +44 (0)1491 838800", style = "color:gray; text-align:justify;"),
             p("F: +44 (0)1491 692424", style = "color:gray; text-align:justify;"),
             p("We are a not-for-profit company limited by guarantee with charitable status. We serve as a strategic delivery partner for the Natural Environment Research Council, part of UK Research and Innovation.", style = "color:gray; text-align:justify;"),
             h5("The name and contact details of our data protection representative"),
             p("The UKCEH Data Protection Officer is Quentin Tucker.", style = "color:gray; text-align:justify;"),
             h5("The retention periods for personal data"),
             p("Personal data retention is guided by the UKCEH retention schedule. Science Research project records may be kept for 10 and 20 years after the project is completed or in exceptional circumstances will be retained permanently.", style = "color:gray; text-align:justify;"),
             h5("The rights available to indiciduals in respect of the processing"),
             p("The GDPR / DPA 2018 provides the following rights for individuals:", style = "color:gray; text-align:justify;"),
             p("  •  The right to be informed", style = "color:gray; text-align:justify;"),
             p("  • The right of access ", style = "color:gray; text-align:justify;"),
             p("	• The right to rectification ", style = "color:gray; text-align:justify;"),
             p("	• The right to erasure ", style = "color:gray; text-align:justify;"),
             p(" 	• The right to restrict processing", style = "color:gray; text-align:justify;"),
             p(" 	• The right to data portability", style = "color:gray; text-align:justify;"),
             p("  • The right to object", style = "color:gray; text-align:justify;"),
             p(" 	• Rights in relation to automated decision-making and profiling", style = "color:gray; text-align:justify;"),
             p("For further details on individual rights, please visit the ICO guide to GDPR.The lawful basis for UKCEH processing personal data can affect which rights are available to individuals. An individual always has the right to object to processing for the purposes of direct marketing, whatever lawful basis applies. The remaining rights are not always absolute, and there are other rights which may be affected in other ways. If UKCEH is relying on legitimate interests more detail will be provided in the privacy notice to comply with the right to be informed.", style = "color:gray; text-align:justify;"),
             p("Further details on how the lawful basis for processing your data affect the rights available to you are outlined below:", style = "color:gray; text-align:justify;"),
             p(" 	• Contract: If we are processing your data on the basis of contract, your right to object and your right not to be subject to a decision based solely on automated processing will not apply. However, you will have a right to data portability.", style = "color:gray; text-align:justify;"),
             p(" 	• Legal obligation: If your data is being processed on the basis of legal obligation, you have no right to erasure, right to data portability, or right to object.", style = "color:gray; text-align:justify;"),
             p(" 	• Public task: Your rights to erasure and data portability do not apply if your data is processed on the basis of public task. However, you do have a right to object.", style = "color:gray; text-align:justify;"),
             p(" 	• Legitimate interest: Where UKCEH is relying on legitimate interests, the right to data portability does not apply.", style = "color:gray; text-align:justify;"),
             h5("The right to withdraw consent (if applicable)"),
             p("Where your personal data is processed using consent as the lawful basis, you have the right to withdraw consent at any time. You will be informed about the ways you can withdraw your consent.", style = "color:gray; text-align:justify;"),
             h5("The right to lodge a complaint with a supervisory authority"),
             p("Initially please raise your concern with UKCEH: please contact the team who process your data. Any continuing concerns you may have can be raised with the UKCEH Data Protection Officer: Quentin Tucker. If UKCEH has not resolved your information rights concern you can raise the matter with the Information Commissioner’s Office via live chat or by phoning +44 (0)303 123 1113.", style = "color:gray; text-align:justify;"),
             h5("Provision of privacy information"),
             p("There are a variety of ways in which UKCEH provides privacy information, including:", style = "color:gray; text-align:justify;"),
             p(" 	• Providing individuals with privacy information at the time we collect their personal data from them.", style = "color:gray; text-align:justify;"),
             p(" 	• If we obtain personal data from a source other than the individual it relates to, we provide them with privacy information,", style = "color:gray; text-align:justify;"),
             p(" 	• within a reasonable of period of obtaining the personal data and no later than one month;", style = "color:gray; text-align:justify;"),
             p(" 	• if we plan to communicate with the individual, we will do this at the latest when the first communication takes place;", style = "color:gray; text-align:justify;"),
             p("  • if we plan to disclose the data to someone else, we will do this at the latest, when the data is disclosed.", style = "color:gray; text-align:justify;"),
             h5("Changes to the information"),
             p("We regularly review and, where necessary, update our privacy information. If we plan to use personal data for a new purpose, we update our privacy information and communicate the changes to individuals before starting any new processing.", style = "color:gray; text-align:justify;"),
             h5("Affiliated and hosted websites"),
             p("Where this privacy notice applies to hosted / affiliated websites, the site will provide a link to this privacy notice, along with any additional privacy information that is applicable.", style = "color:gray; text-align:justify;"),
             h5("Automated technologies or interactions"),
             p("When you use our websites (including but not restricted to, the UKCEH website and our affiliated websites) we may collect the following information about you:", style = "color:gray; text-align:justify;"),
             p(" 	• the IP address used to connect your computer to the Internet", style = "color:gray; text-align:justify;"),
             p(" 	• your time zone setting", style = "color:gray; text-align:justify;"),
             p(" 	• your Internet provider", style = "color:gray; text-align:justify;"),
             p(" 	• your name, address, email address, telephone number, organisation, where you have specifically provided this information on a web submission form.", style = "color:gray; text-align:justify;"),
             p("We may also collect the following information about your visit:", style = "color:gray; text-align:justify;"),
             p(" 	• the full Uniform Resource Locators etc.", style = "color:gray; text-align:justify;"),
             p(" 	• the pages you viewed, including our products and software pages;", style = "color:gray; text-align:justify;"),
             p(" 	• page response times;", style = "color:gray; text-align:justify;"),
             p(" 	• length of visits to certain pages;", style = "color:gray; text-align:justify;"),
             p(" 	• page interaction information (such as clicks, downloads, web form submissions)", style = "color:gray; text-align:justify;"),
             p("We will use this information to provide the best possible service to our web users. It allows us to administer our site, including our efforts to keep it safe and secure, and to carry out internal operations including troubleshooting, data analysis, testing, and statistical research. This means we can improve our websites to ensure that content is presented in the most effective manner for you.", style = "color:gray; text-align:justify;"),
             h5("Cookies"),
             p("Our website uses cookies to distinguish you from other users of our website. This helps us to provide you with a good experience when you browse our website and also allows us to improve our site. For detailed information on the cookies we use and the purposes for which we use them see our cookies policy.", style = "color:gray; text-align:justify;"),
             h5("Links"),
             p("Our site may, from time to time, contain links to and from the websites of our partner networks, advertisers and affiliates. If you follow a link to any of these websites, please note that these websites and any services that may be accessible through them have their own privacy notices and that we do not accept any responsibility or liability for these policies or for any personal data that may be collected through these websites or services. Please check these policies before you submit any personal data to these websites or use these services.", style = "color:gray; text-align:justify;"),
             p("








               ", style = "color:gray; text-align:justify;")





             )
      )
    )
  )
}

#' about Server Functions
#'
#' @noRd
mod_privacy_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns
  })
}
