
package kms.querybus.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "queryKA", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "queryKA", namespace = "http://localhost:8080/", propOrder = {
    "kaIdentifier",
    "procedure",
    "parameters"
})
public class QueryKA {

    @XmlElement(name = "kaIdentifier", namespace = "")
    private String kaIdentifier;
    @XmlElement(name = "procedure", namespace = "")
    private String procedure;
    @XmlElement(name = "parameters", namespace = "", nillable = true)
    private String[][] parameters;

    /**
     * 
     * @return
     *     returns String
     */
    public String getKaIdentifier() {
        return this.kaIdentifier;
    }

    /**
     * 
     * @param kaIdentifier
     *     the value for the kaIdentifier property
     */
    public void setKaIdentifier(String kaIdentifier) {
        this.kaIdentifier = kaIdentifier;
    }

    /**
     * 
     * @return
     *     returns String
     */
    public String getProcedure() {
        return this.procedure;
    }

    /**
     * 
     * @param procedure
     *     the value for the procedure property
     */
    public void setProcedure(String procedure) {
        this.procedure = procedure;
    }

    /**
     * 
     * @return
     *     returns String[][]
     */
    public String[][] getParameters() {
        return this.parameters;
    }

    /**
     * 
     * @param parameters
     *     the value for the parameters property
     */
    public void setParameters(String[][] parameters) {
        this.parameters = parameters;
    }

}
