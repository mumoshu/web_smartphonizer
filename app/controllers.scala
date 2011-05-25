package controllers

import play._
import libs.{MimeTypes, WS}
import play.mvc._
import results.{RenderHtml, RenderXml, RenderBinary}
import xml.transform._
import xml._
import org.htmlcleaner.{SimpleXmlSerializer, HtmlCleaner}

class ReplaceFontTag extends RewriteRule {
  override def transform(n: Node) = n match {
    case e @ Elem(prefix, "font", attributes, scope, child @ _*) => {
      Logger.info(e.toString)
      new Elem(prefix, "span", attributes, scope, child : _*)
    }
    case other => other
  }
}

// Try this sample
// http://sakaki0214.com/sample/091030/
object Application extends Controller {
    
  def index = Template
  def google = {
    Html(WS.url("http://google.com").get().getString)
  }

  private def cleanXml(xml : String): Node = {
    val cleaner = new HtmlCleaner()
    val tagNode = cleaner.clean(xml)
    val resultString = new SimpleXmlSerializer(cleaner.getProperties).getAsString(tagNode)
    Logger.info(resultString);
    XML.loadString(resultString)
  }

  // To test: run "play test" then open the below url in your browser:
  // http://localhost:9000/s/sakaki0214.com/sample/091030/
  def proxy(path: String) = {
    def transform(node: NodeSeq): NodeSeq = {
      if (node isEmpty) {
        node
      } else {
        def mapContent(c: NodeSeq) = c.map(transform(_)).reduceLeft((a, b) => a ++ b)
        node match {
          case <font>{ content @ _* }</font> => <span class="font">{
            mapContent(content)
            }</span>
          case <div>{ content @ _* }</div> => <span>{ mapContent(content) }</span>
          case n => n
        }
      }
    }
    val res = WS.url("http://" + path).getAsync.get()

    Logger.info(res.getContentType)
    if (res.getContentType.startsWith("text/html")) {
      val xml = cleanXml(res.getString)
      val transformed = xml.map(transform(_))
//      Html(transformed)
      Logger.info("text/html")
      Html(new RuleTransformer(new ReplaceFontTag)(xml))
    } else {
      response.contentType = res.getContentType
      new RenderBinary(res.getStream, null, true)
    }
  }
}
