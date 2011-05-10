
package kms.querybus.webservice;

import java.net.MalformedURLException;
import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.1-hudson-28-
 * Generated source version: 2.2
 * 
 */
@WebServiceClient(name = "QueryService", targetNamespace = "http://localhost:8080/", wsdlLocation = "file:/F:/KME%20Project%20Builds/Forks/Open%20Source/KMS-Interface/build/wsdl/QueryService.wsdl")
public class QueryService
    extends Service
{

    private final static URL QUERYSERVICE_WSDL_LOCATION;
    private final static WebServiceException QUERYSERVICE_EXCEPTION;
    private final static QName QUERYSERVICE_QNAME = new QName("http://localhost:8080/", "QueryService");

    static {
        URL url = null;
        WebServiceException e = null;
        try {
            url = new URL("file:/F:/KME%20Project%20Builds/Forks/Open%20Source/KMS-Interface/build/wsdl/QueryService.wsdl");
        } catch (MalformedURLException ex) {
            e = new WebServiceException(ex);
        }
        QUERYSERVICE_WSDL_LOCATION = url;
        QUERYSERVICE_EXCEPTION = e;
    }

    public QueryService() {
        super(__getWsdlLocation(), QUERYSERVICE_QNAME);
    }

    public QueryService(WebServiceFeature... features) {
        super(__getWsdlLocation(), QUERYSERVICE_QNAME, features);
    }

    public QueryService(URL wsdlLocation) {
        super(wsdlLocation, QUERYSERVICE_QNAME);
    }

    public QueryService(URL wsdlLocation, WebServiceFeature... features) {
        super(wsdlLocation, QUERYSERVICE_QNAME, features);
    }

    public QueryService(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public QueryService(URL wsdlLocation, QName serviceName, WebServiceFeature... features) {
        super(wsdlLocation, serviceName, features);
    }

    /**
     * 
     * @return
     *     returns QueryServiceImpl
     */
    @WebEndpoint(name = "QueryPort")
    public QueryServiceImpl getQueryPort() {
        return super.getPort(new QName("http://localhost:8080/", "QueryPort"), QueryServiceImpl.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns QueryServiceImpl
     */
    @WebEndpoint(name = "QueryPort")
    public QueryServiceImpl getQueryPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://localhost:8080/", "QueryPort"), QueryServiceImpl.class, features);
    }

    private static URL __getWsdlLocation() {
        if (QUERYSERVICE_EXCEPTION!= null) {
            throw QUERYSERVICE_EXCEPTION;
        }
        return QUERYSERVICE_WSDL_LOCATION;
    }

}