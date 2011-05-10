
package kms.admin.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "exportKA", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "exportKA", namespace = "http://localhost:8080/", propOrder = {
    "kaIdentifier",
    "format"
})
public class ExportKA {

    @XmlElement(name = "kaIdentifier", namespace = "")
    private String kaIdentifier;
    @XmlElement(name = "format", namespace = "")
    private String format;

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
    public String getFormat() {
        return this.format;
    }

    /**
     * 
     * @param format
     *     the value for the format property
     */
    public void setFormat(String format) {
        this.format = format;
    }

}
