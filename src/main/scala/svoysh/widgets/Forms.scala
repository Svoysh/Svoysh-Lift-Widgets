package svoysh.widgets

import xml._
import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._

/**
 * Helpers to work with HTML forms.
 */
object Forms {

	/**
	 * Create form field without label. Can be used to place submit buttons
	 * on the form where no need in label for a button.
	 */
	def field(input: Node): Elem = {
		field(null, input)
	}

	def field(input: NodeSeq): Elem = {
		field(null, input)
	}
	
	/**
	 * Create labeled form field. input can be SHtml.text
	 */
	def field(label: String, input: Node): Elem = {
		val id = (input \ "@id").toString
		field(label, List(input), id)
	}

	def field(label: String, input: NodeSeq, id: String = null): Elem = {
		val fixedLabel = label match {
			case s: String if s.trim() != "" => s + ":"
			case _ => ""
		}
		val messageList = if (id ne null) messagesById(id)(errors) else Nil
		val hasMessages = messageList.size > 0 
		val cssClass = "Message " + (if (hasMessages) "Error" else "Hidden")
		val messages = messageList match { 
			case list: List[NodeSeq] if hasMessages => { 
				<ul class="FormFieldMessages">{messageList.map(m => <li>{m}</li>)}</ul> 
			} 
			case _ => Nil 
		} 
		<div class={cssClass}>
		<table class="FormField">
			<tr> 
				<td class="FormFieldLabel">
					<label for={id}>{fixedLabel}</label>&nbsp; 
				</td> 
				<td class="FormFieldInput"> 
					{input}{messages} 
				</td> 
			</tr> 
		</table>
		</div>
	}
}