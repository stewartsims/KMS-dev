package kms.knowledgeparser.scanning;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.json.simple.parser.ContainerFactory;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import kms.admin.administration.AdminException;
import kms.prolog.comms.HttpManager;
import kms.prolog.comms.PrologException;
import kms.prolog.comms.PrologRESTClient;

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
 * @author Stewart Sims
 *
 */
public class ScanManager extends PrologRESTClient {
	private Logger logger = Logger.getLogger("kms.knowledgeparser.scanning.ScanManager");
	
	public ScanManager() {
		super();
	}
	
	@SuppressWarnings("unchecked")
	public boolean processNativeLogic(String logic) {
		try {
			String response = httpManager.updateProlog(logic);
			return parseJSONBooleanResponse(response);
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking KA existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		} catch (ParsingException e) {
			logger.log(Level.SEVERE,"Parsing Exception checking procedure existence",e);
			return false;
		} catch (IOException e) {
			logger.log(Level.SEVERE,"IO Exception checking procedure existence",e);
			return false;
		}
	}
}
