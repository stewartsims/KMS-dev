/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package kms.prolog.comms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ejb.EJB;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.namespace.QName;
import javax.xml.rpc.ServiceException;
import javax.xml.ws.Service;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;
import org.json.simple.parser.ContainerFactory;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import kms.transport.soapfeedback.webservice.TransportServiceImpl;
import kms.transport.webservices.Transport;
import kms.transport.webservices.TransportManager;

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
 *
 * TransportServlet
 * An implementation of a RESTful web service for the prolog back-end to communicate with.
 * This is for the purpose of Knowledge Agents retrieving external data.
 * @author Stewart Sims
 */
public class TransportServlet extends HttpServlet {
   
	private Logger logger = Logger.getLogger("kms.prolog.comms.TransportServlet");
	
	public static final String TRANSPORT_PARAMETER_ID = "transportId";
	public static final String TRANSPORT_PARAMETER_QUERY = "queryPayload";
//	public static final String TRANSPORT_FEEDBACK_WSDL = "http://localhost:8080/KMS-Transport/TransportService?wsdl";
//	public static final String TRANSPORT_FEEDBACK_NAMESPACE = "http://localhost:8080/"; 
//	public static final String TRANSPORT_FEEDBACK_SERVICENAME = "TransportService";
//	public static final String TRANSPORT_FEEDBACK_PORTNAME = "TransportPort";
	public static final String TRANSPORT_JSON_OBJECT_RETURN_PARAMETER = "return";
	
	@EJB
	private TransportManager transportManager;
	
    /** 
     *
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
        
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

    /** 
     * Prolog typically deals with HTTP POST for RESTful communication
     * Therefore we use the doPost to implement a RESTful web service
     * for contacting the chosen web service to receive the chosen data
     * Prolog sends JSON { transport: "moduleId", queryPayload="relevant parameters" }
     * This method:
     * 1. Looks up module id in RDBMS in order to find external web service location
     *    to retrieve data from
     * 2. Acts as a SOAP client to retrieve data from this service
     * --> For first web service 
     *     a.Create and generate web service WSDL
     *     b.Use wsimport to generate necessary stubs
     *     c.Import stubs to project
     *     d.Use WSDL as generic wsdl for people to implement other web services
     * @param request servlet request
     * @param response servlet response
     * @throws ServletException if a servlet-specific error occurs
     * @throws IOException if an I/O error occurs
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException {
//    	logger.log(Level.INFO, "Inside the transport servlet");
    	response.setContentType("text/html;charset=UTF-8");
        PrintWriter out = response.getWriter();
        try {
        	ServletInputStream inputStream = request.getInputStream();
        	BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
        	StringBuilder requestStringBuilder= new StringBuilder();
        	String line;
            while ((line = bufferedReader.readLine()) != null) {
              requestStringBuilder.append(line + "\n");
            }
            inputStream.close();
        	PrologRESTClient restClient = new PrologRESTClient();
        	JSONParser jsonParser = new JSONParser();
    		ContainerFactory jsonContainerFactory = getJsonContainerFactory();
        	try {
//        		out.print(requestStringBuilder.toString());
        		Map<String,Object> jsonMap = 
        			(Map<String, Object>) jsonParser.parse(requestStringBuilder.toString(), jsonContainerFactory);
	        	String transportId = (String) jsonMap.get(TRANSPORT_PARAMETER_ID);
	        	Object queryObject = (Object) jsonMap.get(TRANSPORT_PARAMETER_QUERY);
	        	String query = null;
	        	if(queryObject instanceof String) {
	        		query = (String) queryObject;
	        	} else if(queryObject instanceof Map) {
	        		query = JSONValue.toJSONString(queryObject);
	        	}
	        	if(transportId != null) {
    				TransportServiceImpl transportService = getTransportModuleWebService(transportId);
    				if(transportService != null) {
	    				String result = (String) transportService.transport(query);
	    				out.print(result);
    				} else {
    					throw new TransportException("Transport Servlet could not locate transport web service with transportId: " + transportId);
    				}	    				
	        	} else {
	        		throw new TransportException("Transport Servlet not supplied with transportId");
	        	}
        	} catch(ParseException e) {
        		//log and put something in response
        		logger.log(Level.SEVERE, "", e);
        	} catch (ServiceException e) {
        		//log and put something in response
        		logger.log(Level.SEVERE, "", e);
    		} catch (IOException e) {
    			//log and put something in response
        		logger.log(Level.SEVERE, "", e);
        		throw(e);
    		} catch (TransportException e) {
				logger.log(Level.SEVERE, "", e);
			}
        } finally { 
            out.close();
        }
        processRequest(request, response);
    }
    
    public TransportServiceImpl getTransportModuleWebService(String transportId) throws ServiceException, MalformedURLException {
    	if(transportId != null) {
    		Transport transport = transportManager.findById(transportId);
	    	String wsdlURL = transport.getWsdl();
			String namespace = transport.getNamespace();
			String serviceName = transport.getServiceName();
			String portName = transport.getPortName();
			QName serviceQN = new QName(namespace, serviceName);
			QName portQN = new QName(namespace, portName);
			Service requestedService = Service.create(new URL(wsdlURL), serviceQN);
			TransportServiceImpl port = 
				(TransportServiceImpl) requestedService.getPort(portQN, TransportServiceImpl.class);
			return port;
    	} else {
    		return null;
    	}
    }
    
    public ContainerFactory getJsonContainerFactory() {
		ContainerFactory jsonContainerFactory = new ContainerFactory(){
		    public List<Object> creatArrayContainer() {
		      return new LinkedList<Object>();
		    }

		    public Map<String,Object> createObjectContainer() {
		      return new LinkedHashMap<String,Object>();
		    }
		                        
		  };
	   return jsonContainerFactory;
    }
    
    public String jsonResponse(String value) {
    	
    	JSONObject jsonObject = new JSONObject();
    	jsonObject.put("return", value);
    	return value;
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
