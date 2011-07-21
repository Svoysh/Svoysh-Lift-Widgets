package svoysh.widgets

import xml._

import net.liftweb._
import common._
import http._
import util._
import S._
import Helpers._

/**
 * Usage example:
 *
 * class EditShopFormTabs(shop: Shop) extends FormTabs {
 *
 *     def this() {
 *         this(EditShopSnippet.editShop)
 *     }
 *
 *     import shop._
 *
 *     val mainTab = new FormTab {
 *         def displayName = "Required"
 *         addFields(name, currency, country)
 *     }
 *
 *     val contactsTab = new FormTab {
 *         def displayName = "Contact info"
 *         addFields(address, city, state, zip, phone, email, skype, icq)
 *     }
 *
 *     val descTab = new FormTab {
 *         def displayName = "Description"
 *         addFields(desc)
 *     }
 *
 *     addTabs(mainTab, contactsTab, descTab)
 *     activateDefaultTabIfNoneActive
 * }
 */
trait FormTabs {

	var tabs: List[FormTab] = Nil

	def addTab(moreTab: FormTab) {
		addTabs(List(moreTab))
	}

	def addTabs(moreTabs: FormTab*) {
		addTabs(moreTabs.toList)
	}

	def addTabs(moreTabs: List[FormTab]) {
		moreTabs.foreach(_.addToGroup(this))
		tabs = tabs ::: moreTabs
	}

	protected val defaultErrorCount: Int = 0

	/**
	 * Summary count of errors by tab on this tab group.
	 */
	protected var errorCount: Int = defaultErrorCount

	def incErrorCount(additionalErrorCount: Int) {
		errorCount = errorCount + additionalErrorCount
	}

	def getErrorCount: Int = errorCount

	def hasErrors: Boolean = errorCount > 0

	def reset {
		countErrors
	}

	def countErrors {
		errorCount = defaultErrorCount
		var firstTabWithErrorsBox: Box[FormTab] = Empty

		tabs.foreach(tab => {
			val tabErrorCount = tab.countErrors
			if (tabErrorCount > 0) {
				// Activate first tab in group that has errors.
				if (firstTabWithErrorsBox.isEmpty) {
					firstTabWithErrorsBox = Full(tab)
				}
				incErrorCount(tabErrorCount)
			}
		})

		if (firstTabWithErrorsBox.isDefined) {
			if (!hasActiveTabErrors) {
				activateTab(firstTabWithErrorsBox.open_!)
			}
		}
	}

	private var activeTabBox: Box[FormTab] = Empty

	def hasActiveTab: Boolean = activeTabBox.isDefined

	def activeTab: FormTab = activeTabBox.open_!

	def activateTab(tab: FormTab) {
		if (!isActiveTab(tab)) {
			activeTabBox = Full(tab)
		}
	}

	def hasActiveTabErrors: Boolean = hasActiveTab && activeTab.hasErrors

	def isActiveTab(tab: FormTab) = hasActiveTab && (activeTab eq tab)

	def activateTabById(id: String) {
		tabs.find(_.uniqueId == id).foreach(activateTab(_))
	}

	def activateDefaultTabIfNoneActive {
		if (!hasActiveTab && !tabs.isEmpty) {
			activateTab(tabs.head)
		}
	}

	val activeTabHolderCssClass = "ActiveTabHolder"

	def activeTabHolderToHtml: NodeSeq = <div class="Hidden">{SHtml.text(
		(if (hasActiveTab) activeTab.uniqueId else ""),
		(activateTabById _),
		("class", activeTabHolderCssClass)
	)}</div>

	def tabBarToHtml: NodeSeq = <div class="FormTabBar">{tabs.flatMap(tab => {
		<a href="javascript://" class={tab.tabCssClasses.mkString(" ")}
			onclick={"selectTab('" + tab.uniqueId + "');" }>{tab.displayName ++
				tab.fieldErrorCountToHtml}</a>
	})}</div>

	def tabsToHtml: NodeSeq = tabs.flatMap(tab => {tab.toHtml})

	def jsToHtml: NodeSeq = <script type="text/javascript">{
// TODO: optimize: Next code can be extracted to separate JS file and packed into a module.
// TODO: ui: Add history support by URL #!. See for some jQuery plugins.
// [http://tkyk.github.com/jquery-history-plugin/]
"""
var prevTabSelector = """ + (if (hasActiveTab) ("'." + activeTab.uniqueId + "'") else "null") + """;
var activeTabHolderSelector = '.""" + activeTabHolderCssClass + """';

function selectTab(tabId) {
	var tabSelector = '.' + tabId;
	if (tabSelector === prevTabSelector) {
		return;
	}
	if (prevTabSelector !== null) {
		jQuery(prevTabSelector).removeClass('Active');
	}
	prevTabSelector = tabSelector;
	jQuery(tabSelector).addClass('Active').blur();
	jQuery(activeTabHolderSelector).val(tabId);
}
"""}</script>

	def toHtml: NodeSeq = {
		reset
		(
			activeTabHolderToHtml ++
			tabBarToHtml ++
			tabsToHtml ++
			jsToHtml
		)
	}

	reset
}

trait FormTab {

	// TODO: ui: Add optional (Box) tab help/description block to tell user about
	// purposes tab.

	lazy val uniqueId: String = "FormTab" + randomString(10)

	var fields: List[FormField] = Nil

	def addFields(moreFields: FormField*) {
		addFields(moreFields.toList)
	}

	def addFields(moreFields: List[FormField]) {
		fields = fields ::: moreFields
	}

	protected var errorCountBox: Box[Int] = Empty

	def getErrorCount: Int = {
		if (errorCountBox.isEmpty) {
			countErrors
		}
		errorCountBox.openOr(0)
	}

	def hasErrors: Boolean = getErrorCount > 0

	/**
	 *  Summary count of errors by every field on this tab.
	 */
	def countErrors: Int = {
		val errorCount: Int = fields.foldLeft(0)((count, field) => {
			count + messagesById(field.uniqueId)(errors).length
		})
		errorCountBox = Full(errorCount)
		errorCount
	}

	def fieldErrorCountToHtml: NodeSeq = {
		if (hasErrors) {
			val errorCount = getErrorCount
			<div class="ErrorCountInFormTab" title={"Errors on this tab: " +
				errorCount}>{errorCount}</div>
		}
		else Nil
	}

	def displayName: String

	var tabGroupBox: Box[FormTabs] = Empty

	def hasTabGroup: Boolean = tabGroupBox.isDefined

	def tabGroup: FormTabs = tabGroupBox.open_!

	def addToGroup(tabGroup: FormTabs) {
		this.tabGroupBox = Full(tabGroup)
	}

	def isActive: Boolean = (hasTabGroup && tabGroup.hasActiveTab &&
		(tabGroup.activeTab eq this))

	def baseCssClasses: List[String] = {
		uniqueId :: (if (isActive) "Active" else "") :: Nil
	}

	def tabCssClasses: List[String] = "FormTab" :: baseCssClasses

	def contentCssClasses: List[String] = "FormTabContent" :: baseCssClasses

	def fieldsToHtml: NodeSeq = fields.flatMap(_.toHtml.openOr(Nil))

	def toHtml: NodeSeq = {
		<div class={contentCssClasses.mkString(" ")}>{fieldsToHtml}</div>
	}
}

trait FormField {

	lazy val uniqueId: String = newUniqueId

	def newUniqueId: String = ("FormField" + randomString(10))

	def toHtml: Box[NodeSeq]
}

object FormField {

	implicit def baseFieldToFormField(baseField: BaseField): FormField = {
		new FormField {
			override lazy val uniqueId = baseField.uniqueFieldId.openOr(newUniqueId)
			def toHtml = baseField.toForm
		}
	}

	implicit def baseFieldsToFormFields(baseFields: BaseField*): List[FormField] = {
		baseFieldsToFormFields(baseFields.toList)
	}

	implicit def baseFieldsToFormFields(baseFields: List[BaseField]): List[FormField] = {
		baseFields.map(baseFieldToFormField(_))
	}
}