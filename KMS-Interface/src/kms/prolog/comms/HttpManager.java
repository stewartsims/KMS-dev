package kms.prolog.comms;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.Socket;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.util.logging.Level;
import java.util.logging.Logger;

import kms.knowledgeparser.scanning.ParsingException;

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
 * HttpManager
 * This is a mechanism used to send all the JSON requests via HTTP required to carry out modification
 * querying and administration of the prolog back-end. It essentially acts as a RESTful web service client
 * @author Stewart Sims
 *
 */
public class HttpManager {
	
	private static String KMS_BACKEND_HOSTNAME="localhost";
	private static int KMS_BACKEND_PORT=5000;
	private static int SOCKET_TIMEOUT=5000;
	private static String QUERY_RESOURCE="/swapp/kms/db";
	private static String UPDATE_RESOURCE="/swapp/kms/db";
	private static String KA_MANAGER_MODULE = "ka_manager";
	private static String KA_MANAGER_PASSWORD = "adminpassword";
	private static String ADMIN_RESOURCE="/swapp/admin?module="+KA_MANAGER_MODULE+
								 "&password="+KA_MANAGER_PASSWORD+
								 "&query=";
	private Logger logger = Logger.getLogger("kms.prolog.comms.HttpManager");

	public synchronized String updateProlog(String update) 
		throws ParsingException, IOException {
		HttpURLConnection connection = null;
	    OutputStreamWriter writer = null;
	    BufferedReader reader  = null;
	    StringBuilder stringBuilder = null;
	    String line = null;
	    URL serverAddress = null;
	    try {
	    	String url = "http://"+KMS_BACKEND_HOSTNAME+":"+KMS_BACKEND_PORT+UPDATE_RESOURCE;
	        serverAddress = new URL(url);
	        connection = null;
	        connection = (HttpURLConnection)serverAddress.openConnection();
	        connection.setRequestMethod("POST");
	        connection.setRequestProperty("Accept","application/json");
	        connection.setRequestProperty("Content-Type","application/jsonrequest");
	        connection.setDoInput(true);
	        connection.setDoOutput(true);
	        connection.setReadTimeout(SOCKET_TIMEOUT);
	        connection.connect();
	        writer = new OutputStreamWriter (
	                    connection.getOutputStream ());
	        writer.write(update);
	        writer.flush();
	        writer.close();
	        reader  = new BufferedReader(new InputStreamReader(connection.getInputStream()));
	        stringBuilder = new StringBuilder();
	        while ((line = reader.readLine()) != null) {
	            stringBuilder.append(line);
	        } 
	        if(stringBuilder.toString()!=null) {
	        	return stringBuilder.toString();
	        } else {
	        	throw new ParsingException("SWAPP-KMS response body not found");
	        }
	     } catch(IOException e) {
	      	  if(connection == null || connection.getErrorStream() == null) {
	      		  throw e;
	      	  } else {
		    	  reader  = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
			         stringBuilder = new StringBuilder();
			         while ((line = reader.readLine()) != null) {
			             stringBuilder.append(line);
			         } 
			         if(stringBuilder.toString()!=null) {
			        	 logger.log(Level.SEVERE, "HttpManager reponse: " + stringBuilder.toString());
			         }
		    	  throw(e);  
	      	  }
	      } finally {
	    	  if(connection != null) {
	    		  connection.disconnect();
	    	  }
	          reader = null;
	          stringBuilder = null;
	          connection = null;
	      }
	}
	
	public synchronized String queryPrologGET(String kaIdentifier, String logic) 
		throws ParsingException, IOException {
		 HttpURLConnection connection = null;
	     BufferedReader reader  = null;
	     StringBuilder stringBuilder = null;
	     String line = null;
	     URL serverAddress = null;
	     try {
	    	 logic = URLEncoder.encode(logic,"UTF-8");
	    	 String url = "http://"+KMS_BACKEND_HOSTNAME+":"+KMS_BACKEND_PORT+QUERY_RESOURCE;
	    	 String parameters = "?query="+logic+"&module="+kaIdentifier;
	         serverAddress = new URL(url+parameters);
	         connection = null;
	         connection = (HttpURLConnection)serverAddress.openConnection();
	         connection.setRequestMethod("GET");
	         connection.setRequestProperty("Accept","application/json");
	         connection.setRequestProperty("Content-Type","application/jsonrequest");
	         connection.setDoOutput(true);
	         connection.setReadTimeout(SOCKET_TIMEOUT);
	         connection.connect();
	         reader  = new BufferedReader(new InputStreamReader(connection.getInputStream()));
	         stringBuilder = new StringBuilder();
	         while ((line = reader.readLine()) != null) {
	             stringBuilder.append(line);
	         } 
	         if(stringBuilder.toString()!=null) {
	        	 return stringBuilder.toString();
	         } else {
	        	 throw new ParsingException("SWAPP-KMS response body not found");
	         }
	      } catch(IOException e) {
	      	  if(connection == null || connection.getErrorStream() == null) {
	      		  throw e;
	      	  } else {
		    	  reader  = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
			         stringBuilder = new StringBuilder();
			         while ((line = reader.readLine()) != null) {
			             stringBuilder.append(line);
			         } 
			         if(stringBuilder.toString()!=null) {
			        	 logger.log(Level.SEVERE, "HttpManager reponse: " + stringBuilder.toString());
			         }
		    	  throw(e);  
	      	  }
	      } finally {
	    	  if(connection != null) {
	    		  connection.disconnect();
	    	  }
	          reader = null;
	          stringBuilder = null;
	          connection = null;
	      }
	  }
	
	public synchronized String queryPrologPOST(String kaIdentifier, String logic) 
		throws ParsingException, IOException {
		 HttpURLConnection connection = null;
	     BufferedReader reader  = null;
	     DataOutputStream dataOutputStream = null;
	     StringBuilder stringBuilder = null;
	     String line = null;
	     URL serverAddress = null;
	     try {
	    	 String url = "http://"+KMS_BACKEND_HOSTNAME+":"+KMS_BACKEND_PORT+QUERY_RESOURCE;
	    	 String query = kaIdentifier + ":" + logic;
	         serverAddress = new URL(url);
	         connection = null;
	         connection = (HttpURLConnection)serverAddress.openConnection();
	         connection.setRequestMethod("POST");
	         connection.setRequestProperty("Accept","application/json");
	         connection.setRequestProperty("Content-Type","application/jsonrequest");
	         connection.setRequestProperty("Content-Length", "" + 
	                 Integer.toString(query.getBytes().length));
	         connection.setDoInput(true);
	         connection.setDoOutput(true);
	         connection.setReadTimeout(SOCKET_TIMEOUT);
	         connection.setUseCaches(false);
	         
	         dataOutputStream = new DataOutputStream(connection.getOutputStream());
		     dataOutputStream.writeBytes(query);
		     dataOutputStream.flush();
		     dataOutputStream.close();
		     
	         reader  = new BufferedReader(new InputStreamReader(connection.getInputStream()));
	         stringBuilder = new StringBuilder();
	         while ((line = reader.readLine()) != null) {
	             stringBuilder.append(line);
	         } 
	         if(stringBuilder.toString()!=null) {
	        	 return stringBuilder.toString();
	         } else {
	        	 throw new ParsingException("SWAPP-KMS response body not found");
	         }
	      } catch(IOException e) {
	      	  if(connection == null || connection.getErrorStream() == null) {
	      		  throw e;
	      	  } else {
		    	  reader  = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
			         stringBuilder = new StringBuilder();
			         while ((line = reader.readLine()) != null) {
			             stringBuilder.append(line);
			         } 
			         if(stringBuilder.toString()!=null) {
			        	 logger.log(Level.SEVERE, "HttpManager reponse: " + stringBuilder.toString());
			         }
		    	  throw(e);  
	      	  }
	      } finally {
	    	  if(connection != null) {
	    		  connection.disconnect();
	    	  }
	          reader = null;
	          stringBuilder = null;
	          connection = null;
	      }
	}
	
	public synchronized String admin(String query) 
		throws ParsingException, IOException {
		HttpURLConnection connection = null;
	    BufferedReader reader  = null;
	    StringBuilder stringBuilder = null;
	    String line = null;
	    URL serverAddress = null;
	    try {
	    	String url = "http://"+KMS_BACKEND_HOSTNAME+":"+KMS_BACKEND_PORT+ADMIN_RESOURCE;
	    	String parameters = query;
	        serverAddress = new URL(url+parameters);
	        connection = null;
	        connection = (HttpURLConnection)serverAddress.openConnection();
	        connection.setRequestMethod("GET");
	        connection.setRequestProperty("Accept","application/json");
	        connection.setRequestProperty("Content-Type","application/jsonrequest");
	        connection.setDoOutput(true);
	        connection.setReadTimeout(SOCKET_TIMEOUT);
	        connection.connect();
	        reader  = new BufferedReader(new InputStreamReader(connection.getInputStream()));
	        stringBuilder = new StringBuilder();
	        while ((line = reader.readLine()) != null) {
	            stringBuilder.append(line);
	        } 
	        if(stringBuilder.toString()!=null) {
	        	return stringBuilder.toString();
	        } else {
	        	throw new ParsingException("SWAPP-KMS response body not found");
	        }
	     } catch(IOException e) {
	      	  if(connection == null || connection.getErrorStream() == null) {
	      		  throw e;
	      	  } else {
		    	  reader  = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
			         stringBuilder = new StringBuilder();
			         while ((line = reader.readLine()) != null) {
			             stringBuilder.append(line);
			         } 
			         if(stringBuilder.toString()!=null) {
			        	 logger.log(Level.SEVERE, "HttpManager reponse: " + stringBuilder.toString());
			         }
		    	  throw(e);  
	      	  }
	      } finally {
	    	  if(connection != null) {
	    		  connection.disconnect();
	    	  }
	          reader = null;
	          stringBuilder = null;
	          connection = null;
	      }
	}

}
