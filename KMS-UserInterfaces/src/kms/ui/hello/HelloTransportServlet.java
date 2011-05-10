package kms.ui.hello;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import kms.querybus.webservice.AnyTypeArray;
import kms.querybus.webservice.QueryServiceImpl;
import kms.querybus.webservice.StringArray;
import kms.serviceclients.hello.InterfaceServiceClient;

/**
 * Copyright 2010 Stewart Sims
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 * @Author Stewart Sims
 */
public class HelloTransportServlet extends HttpServlet {
 
	private static Logger logger = Logger.getLogger("kms.ui.hello.HelloServlet");
    /** 
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
    	
    	response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        try {
        	//Retrieving the web service
        	QueryServiceImpl queryService  = InterfaceServiceClient.getInterfaceWebService();
        	
        	//Setting up the parameters
        	String name = request.getParameter("name");
        	List<StringArray> parameters = new ArrayList<StringArray>();
        	StringArray helloIn = new StringArray();
			helloIn.getItem().add("Name");	//StringArray with parameter name as first element
			helloIn.getItem().add("'"+name+"'"); //quotes enclose the parameter to tell prolog it is a string
			parameters.add(helloIn);
			
			//The web service call
		   	List<AnyTypeArray> queryResult = 
		   		queryService.queryKA(InterfaceServiceClient.INTERFACE_PARAMETER_KA_IDENTIFIER,
		   							 InterfaceServiceClient.INTERFACE_PARAMETER_HELLO_TRANSPORT_PROCEDURE,
		   							 parameters);
		   	
		   	//Processing the output
		   	List<Object> objects = queryResult.get(0).getItem();
		   	String parameterName = (String) objects.get(0);
		   	if(parameterName.equals("validationMessage")) {
		   		out.print("<html><head></head><body><h1>An error occured: " + (String) objects.get(1) + "</h1></body></html>");
		   	} else {
		   		String responseString = (String) objects.get(1);  
		   		out.print("<html><head></head><body><h1>" + responseString + "</h1></body></html>");
		   	}
        } catch(Exception e) {
        	out.print("<html><head></head><body><h1>Error: " + e + "</h1></body></html>");
        	logger.log(Level.SEVERE, "", e);
        }
    }

    /** 
     * Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        processRequest(request, response);
    } 

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
    	processRequest(request, response);
    }

    /** 
     * Returns a short description of the servlet.
     * @return a String containing servlet description
     */
    @Override
    public String getServletInfo() {
        return "Short description";
    }

}
