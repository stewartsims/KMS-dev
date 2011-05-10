
package kms.querybus.webservice;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the kms.querybus.webservice package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _QueryKAResponse_QNAME = new QName("http://localhost:8080/", "queryKAResponse");
    private final static QName _TestReturn_QNAME = new QName("http://localhost:8080/", "testReturn");
    private final static QName _QueryKA_QNAME = new QName("http://localhost:8080/", "queryKA");
    private final static QName _TestReturnResponse_QNAME = new QName("http://localhost:8080/", "testReturnResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: kms.querybus.webservice
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link TestReturn }
     * 
     */
    public TestReturn createTestReturn() {
        return new TestReturn();
    }

    /**
     * Create an instance of {@link TestReturnResponse }
     * 
     */
    public TestReturnResponse createTestReturnResponse() {
        return new TestReturnResponse();
    }

    /**
     * Create an instance of {@link QueryKA }
     * 
     */
    public QueryKA createQueryKA() {
        return new QueryKA();
    }

    /**
     * Create an instance of {@link AnyTypeArray }
     * 
     */
    public AnyTypeArray createAnyTypeArray() {
        return new AnyTypeArray();
    }

    /**
     * Create an instance of {@link StringArray }
     * 
     */
    public StringArray createStringArray() {
        return new StringArray();
    }

    /**
     * Create an instance of {@link QueryKAResponse }
     * 
     */
    public QueryKAResponse createQueryKAResponse() {
        return new QueryKAResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryKAResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://localhost:8080/", name = "queryKAResponse")
    public JAXBElement<QueryKAResponse> createQueryKAResponse(QueryKAResponse value) {
        return new JAXBElement<QueryKAResponse>(_QueryKAResponse_QNAME, QueryKAResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TestReturn }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://localhost:8080/", name = "testReturn")
    public JAXBElement<TestReturn> createTestReturn(TestReturn value) {
        return new JAXBElement<TestReturn>(_TestReturn_QNAME, TestReturn.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryKA }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://localhost:8080/", name = "queryKA")
    public JAXBElement<QueryKA> createQueryKA(QueryKA value) {
        return new JAXBElement<QueryKA>(_QueryKA_QNAME, QueryKA.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TestReturnResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://localhost:8080/", name = "testReturnResponse")
    public JAXBElement<TestReturnResponse> createTestReturnResponse(TestReturnResponse value) {
        return new JAXBElement<TestReturnResponse>(_TestReturnResponse_QNAME, TestReturnResponse.class, null, value);
    }

}
