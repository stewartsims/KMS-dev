
package kms.querybus.webservice;

import java.util.List;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.ws.Action;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.1-hudson-28-
 * Generated source version: 2.2
 * 
 */
@WebService(name = "QueryServiceImpl", targetNamespace = "http://localhost:8080/")
@XmlSeeAlso({
    ObjectFactory.class
})
public interface QueryServiceImpl {


    /**
     * 
     * @param kaIdentifier
     * @param parameters
     * @param procedure
     * @return
     *     returns java.util.List<kms.querybus.webservice.AnyTypeArray>
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "queryKA", targetNamespace = "http://localhost:8080/", className = "kms.querybus.webservice.QueryKA")
    @ResponseWrapper(localName = "queryKAResponse", targetNamespace = "http://localhost:8080/", className = "kms.querybus.webservice.QueryKAResponse")
    @Action(input = "http://localhost:8080/QueryServiceImpl/queryKARequest", output = "http://localhost:8080/QueryServiceImpl/queryKAResponse")
    public List<AnyTypeArray> queryKA(
        @WebParam(name = "kaIdentifier", targetNamespace = "")
        String kaIdentifier,
        @WebParam(name = "procedure", targetNamespace = "")
        String procedure,
        @WebParam(name = "parameters", targetNamespace = "")
        List<StringArray> parameters);

    /**
     * 
     * @return
     *     returns java.util.List<kms.querybus.webservice.StringArray>
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "testReturn", targetNamespace = "http://localhost:8080/", className = "kms.querybus.webservice.TestReturn")
    @ResponseWrapper(localName = "testReturnResponse", targetNamespace = "http://localhost:8080/", className = "kms.querybus.webservice.TestReturnResponse")
    @Action(input = "http://localhost:8080/QueryServiceImpl/testReturnRequest", output = "http://localhost:8080/QueryServiceImpl/testReturnResponse")
    public List<StringArray> testReturn();

}