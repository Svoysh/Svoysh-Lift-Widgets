package svoysh.widgets

import xml._

import net.liftweb._
import common._
import mapper._
import http._

import svoysh.util._

// TODO: extract hardcoded texts to properties file.

/**
 * Usage example:
 *
 * val pager = new FuzzyPager {
 *     pagePrefixUrl = "/search"
 * }
 * // Some SQL querying using pager.getDbOffset, pager.resultsPerPage, etc...
 * pager.setResultVals(products)
 * val pagerHtml = pager.toHtml
 */
trait FuzzyPager extends Pager {

	override def getAllResults: Long = {
		((currentPage - firstPage) * resultsPerPage) + currentResults
	}

	override def hasMoreThanOnePage = true

	override def getPrevPage = {
		if (currentPage > firstPage) currentPage - 1
		else firstPage
	}

	override def getNextPage = {
		if (hasCurrentResults) currentPage + 1
		else currentPage
	}
}

/**
 * Usage example:
 *
 * val pager = new Pager {
 *     pagePrefixUrl = "/product/list"
 *     includePageInUri = true
 *     setAllResults(productCount)
 * }
 * val pagerHtml = pager.toHtml
 */
trait Pager {

	// Attention! The order of fields makes sense because on field
	// depends on each other and so on.

	var firstPage: Long = 1
	var currentPage: Long = getCurrentPage
	var defaultResultsPerPage: Long = if (isProdMode) 20 else 3
	var resultsPerPage: Long = getResultsPerPage
	private var allResults: Long = 1

	var pagePrefixUrl: String = "/"
	var includePageInUri: Boolean = false
	var reuseRequestParams: Boolean = true

	/** Names of request params to exclude from page URL. */
	var excludeRequestParams: Box[List[String]] = Empty

	val minDbOffset: Long = 0

	private var resultVals: List[_] = Nil

	def getResultVals: List[_] = ?!(resultVals, Nil)

	def setResultVals(list: List[_]) {
		resultVals = list
	}

	def currentResults: Long = getResultVals.size

	def hasCurrentResults: Boolean = currentResults > 0L

	def getAllResults: Long = allResults

	def setAllResults(n: Long) {
		allResults = n
	}

	def getAllPages: Long = {
		if (cache.allPages.isEmpty) {
			var result = getAllResults / resultsPerPage
			if ((getAllResults % resultsPerPage) != 0) result += 1
			cache.allPages = Full(result)
		}
		cache.allPages.open_!
	}

	def isPageInValidRange(page: Long = currentPage): Boolean = {
		page >= firstPage && page <= getAllPages
	}

	def getPrevPage: Long = {
		if (currentPage > firstPage) {
			val lastPage = getLastPage
			if (currentPage > lastPage) lastPage
			else currentPage - 1
		}
		else firstPage
	}

	def getNextPage: Long = {
		if (currentPage < getLastPage) {
			if (currentPage < firstPage) firstPage
			else currentPage + 1
		}
		else currentPage
	}

	def getLastPage: Long = getAllPages - firstPage + 1

	/**
	 * Alias for getCurrentPage().
	 */
	def getCurrPage: Long = getCurrentPage

	def getCurrentPage: Long = {
		var page = getParamAsLong("page", firstPage)
		if (page < firstPage) page = firstPage
		page
	}

	def getResultsPerPage: Long = getParamAsLong("results", defaultResultsPerPage)

	def hasMoreThanOnePage: Boolean = getAllResults > resultsPerPage

	def getDbOffset: Long = resultsPerPage * (currentPage - firstPage) match {
		case n if n > minDbOffset => n
		case _ => minDbOffset
	}

	/** Returns QueryParam-s that can be used in functions like MetaMapper.findAll() */
	def paginateQueryParams[O <: Mapper[O]]: Seq[QueryParam[O]] = {
		val pageRelatedQueryParams: Seq[QueryParam[O]] = List(
			StartAt(getDbOffset), MaxRows(resultsPerPage))
		pageRelatedQueryParams
	}

	def getUrlParams: Map[String, List[String]] = {
		if (reuseRequestParams) S.request.open_!.params
		else Map()
	}

	def paramsToUrlPart: String = {
		val defaultResult = ""
		val params = getUrlParams
		if (params.isEmpty) return defaultResult

		def canAppendParam(paramName: String): Boolean = !cannotAppendParam(paramName)

		def cannotAppendParam(paramName: String): Boolean = {
			("page" == paramName) || (excludeRequestParams.isDefined &&
				excludeRequestParams.open_!.contains(paramName))
		}

		val paramParts = for {
			paramNameAndValues <- params
			paramName = paramNameAndValues._1
			if canAppendParam(paramName)
			paramValue <- paramNameAndValues._2
		} yield paramName + "=" + paramValue

		if (paramParts.nonEmpty) {
			paramParts.reduceLeft((result, paramEqValue) =>
				result + "&" + paramEqValue)
		} else defaultResult
	}

	def getUrlForPage(page: Long): String = {
		if (cache.uriPart.isEmpty) {
			val uriPart = pagePrefixUrl + (if (includePageInUri) "/page/" else "")
			cache.uriPart = Full(uriPart)
		}
		if (cache.paramsPart.isEmpty) {
			val uriPart = cache.uriPart.open_!
			var paramsPart = paramsToUrlPart
			if (!includePageInUri) {paramsPart += (if (!paramsPart.isEmpty && !paramsPart.endsWith("&")) "&" else "") + "page="}
			if (!paramsPart.isEmpty && !uriPart.contains("?")) {paramsPart = "?" + paramsPart}
			cache.paramsPart = Full(paramsPart)
		}
		val pageStr = page.toString
		cache.uriPart.open_! + (if (includePageInUri) pageStr else "") +
			cache.paramsPart.open_! + (if (!includePageInUri) pageStr else "")
	}

	/** Max count of visible pages per result. */
	val maxVisiblePages: Long = 9

	def getPagesToPrint: Seq[Long] = {
		val pageCount: Long = getAllPages
		val page: Long = currentPage
		val fullSize: Long = maxVisiblePages
		val leftSize: Long = fullSize / 2
		val rightSize: Long = fullSize - leftSize - 1

		var beginPage: Long = firstPage
		var endPage: Long = pageCount
		if (fullSize < pageCount) {
			if (page <= (1 + leftSize)) {
				endPage = fullSize
			} else if (page >= (pageCount - rightSize)) {
				beginPage = pageCount - fullSize + 1
			} else {
				beginPage = page - leftSize
				endPage = page + rightSize
			}
		}
		beginPage to endPage
	}

	def pageToHtml(page: Long, pageName: Box[String] = Empty): NodeSeq = {
		val fixedPageName = pageName.openOr(page.toString)
		if (page == currentPage) <span class="CurrentPage">{fixedPageName}</span>
		else <a class="PageLink" href={getUrlForPage(page)}>{fixedPageName}</a>
	}

	def toHtml: NodeSeq = {
		val pageRange = getPagesToPrint
		val result =
		if (hasMoreThanOnePage && pageRange.size > 0) {
			// TODO: ui: Limit visible pages per screen say up to 5.
			<div>{/*<div>Page {currentPage} of {getAllPages}</div>*/}
				<ul class="PageList"><li class="PageItem" title="Previous page">{
				pageToHtml(getPrevPage, Full("<"))}</li><li class="PageItem"
					style="margin-right: 0.5em;" title="Next page">{
				pageToHtml(getNextPage, Full(">"))}</li>{
				pageRange.map(page => <li class="PageItem">{pageToHtml(page)}</li>)
				}</ul>{
			}</div>
		} else Nil
		resetCache
		result
	}

	private lazy val cache = new PagerCache

	def resetCache = cache.reset

	class PagerCache {

		/** Example: [/shop/list{/page}] */
		var uriPart: Box[String] = Empty

		/** Example: [?userId=1&countryCode=US{&page=}] */
		var paramsPart: Box[String] = Empty

		var allPages: Box[Long] = Empty

		def reset {
			uriPart = Empty
			paramsPart = Empty
			allPages = Empty
		}
	}
}