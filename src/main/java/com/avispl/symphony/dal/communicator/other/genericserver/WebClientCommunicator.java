/*
 * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.UUID;
import java.util.regex.Matcher;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.ProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.api.dal.monitor.Monitorable;
import com.avispl.symphony.dal.communicator.RestCommunicator;
import com.avispl.symphony.dal.communicator.other.genericserver.utils.HttpStatus;
import com.avispl.symphony.dal.communicator.other.genericserver.utils.WebClientConstant;
import com.avispl.symphony.dal.util.StringUtils;

/**
 * This class checks accessible to a given URI and support parsing data from JSON or XML content then return the HTTP status code, content type, and monitoring data to Symphony
 *
 * @author Duy Nguyen, Ivan
 * @version 1.0.0
 * @since 1.2.0
 */
public class WebClientCommunicator extends RestCommunicator implements Monitorable {

	/**
	 * URI string that is used to check accessible.
	 *
	 * @since 1.2.0
	 */
	private String URI;

	/**
	 * The exclude is configuration properties on the symphony portal
	 *
	 * @since 1.2.0
	 */
	private String exclude;
	private final List<String> excludedList = new ArrayList<>();

	/**
	 * The parseContent is configuration properties on the symphony portal
	 *
	 * @since 1.2.0
	 */
	private String parseContent;
	private boolean isParseContent;

	private String baseRequestUrl;
	private final ObjectMapper mapper = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION);
	private final DocumentBuilder documentBuilder = buildSecureDocumentBuilder();

	// Using the UUID for separate the response to make sure we do not have any conflict
	private final String statusAndBodySeparator = UUID.randomUUID().toString().replace(WebClientConstant.DASH, "");
	private final String bodyAndContentTypeSeparator = UUID.randomUUID().toString().replace(WebClientConstant.DASH, "");

	/**
	 * WebClientCommunicator instantiation
	 */
	public WebClientCommunicator() throws ParserConfigurationException {
		// WebClientCommunicator no-args constructor
	}

	/**
	 * Retrieves {@code {@link #URI}}
	 *
	 * @return value of {@link #URI}
	 */
	public String getURI() {
		return URI;
	}

	/**
	 * Retrieves {@code {@link #exclude }}
	 *
	 * @return value of {@link #exclude}
	 * @since 1.2.0
	 */
	public String getExclude() {
		return exclude;
	}

	/**
	 * Retrieves {@code {@link #parseContent }}
	 *
	 * @return value of {@link #parseContent}
	 * @since 1.2.0
	 */
	public String getParseContent() {
		return parseContent;
	}

	/**
	 * Sets {@code URI}
	 *
	 * @param URI the {@code java.lang.String} field
	 */
	public void setURI(String URI) {
		this.URI = URI;
	}

	/**
	 * Sets {@code exclude} and set excludes after getExclude from the configuration properties on the symphony portal
	 *
	 * @param exclude the {@code java.lang.String} field
	 * @since 1.2.0
	 */
	public void setExclude(String exclude) {
		this.exclude = exclude;
	}

	/**
	 * Sets {@code Content}
	 * If set to “False” the adapter will not parse the content (regardless of the content type)
	 * If set to “True” the adapter will parse content as indicated in this story.
	 *
	 * @param parseContent the {@code java.lang.String} field
	 * @since 1.2.0
	 */
	public void setParseContent(String parseContent) {
		this.parseContent = parseContent;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adding the logic for build base URL
	 */
	@Override
	protected void internalInit() throws Exception {
		super.internalInit();
		buildBaseUrl();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * WebClientCommunicator doesn't require authentication
	 */
	@Override
	protected void authenticate() {
		// WebClientCommunicator doesn't require authentication
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Get data from uri path
	 *
	 * @param uri the uri is the path get from the configuration properties on the symphony portal
	 * @return String This returns the status code and dataBody if the response get body not null
	 * @throws Exception if getting information from the Uri failed
	 */
	@Override
	public String doGet(String uri) throws Exception {
		HttpClient client = this.obtainHttpClient(false);

		String getUri = this.buildRequestUrl(uri);
		if (this.logger.isDebugEnabled()) {
			this.logger.debug("Performing a GET operation for " + getUri);
		}

		StringBuilder stringBuilder = new StringBuilder();
		HttpResponse response = null;
		try {
			response = client.execute(new HttpGet(getUri));
			// status code
			stringBuilder.append(response.getStatusLine().getStatusCode());

			HttpEntity httpEntity = response.getEntity();
			if (isParseContent && httpEntity != null) {
				// response body
				String dataBody = EntityUtils.toString(httpEntity);
				if (!StringUtils.isNullOrEmpty(dataBody)) {
					stringBuilder.append(statusAndBodySeparator);
					stringBuilder.append(dataBody);
				}

				// content type
				stringBuilder.append(bodyAndContentTypeSeparator);
				stringBuilder.append(getContentType(httpEntity));
			}
		} finally {
			if (response instanceof CloseableHttpResponse) {
				((CloseableHttpResponse) response).close();
			}
		}
		return stringBuilder.toString();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method is called by Symphony to get the list of statistics to be displayed
	 *
	 * @return List<Statistics> This returns the list of statistics
	 */
	@Override
	public List<Statistics> getMultipleStatistics() {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Perform doGet() at host %s with port %s", this.host, this.getPort()));
		}
		final ExtendedStatistics extStats = new ExtendedStatistics();
		final Map<String, String> stats = new HashMap<>();
		String uriStatusMessage;
		if (!StringUtils.isNullOrEmpty(this.URI)) {
			int statusCode;
			try {
				isParseContent = isSupportParseContent();

				String response = doGet(this.URI); // STATUS <statusAndBodySeparator> BODY <bodyAndContentTypeSeparator> CONTENT-TYPE
				int startOfBodyIndex = response.indexOf(statusAndBodySeparator);
				int startOfContentTypeIndex = response.indexOf(bodyAndContentTypeSeparator);

				if (startOfContentTypeIndex < 0) {
					// no content type (means having no response body as well)
					statusCode = Integer.parseInt(response);
				} else {
					// having content type
					if (startOfBodyIndex < 0) {
						// no response body
						statusCode = Integer.parseInt(response.substring(0, startOfContentTypeIndex));
					} else {
						// have response body
						statusCode = Integer.parseInt(response.substring(0, startOfBodyIndex));
					}
				}
				if (!HttpStatus.containsKey(statusCode)) {
					throw new ResourceNotReachableException("Response status code not in range");
				}

				if (isParseContent) {
					String contentType = response.substring(startOfContentTypeIndex + bodyAndContentTypeSeparator.length());
					if (isValidContentType(contentType)) {
						String responseBody = null;
						if (startOfBodyIndex > 0) {
							// have response body
							responseBody = response.substring(startOfBodyIndex + statusAndBodySeparator.length(), startOfContentTypeIndex);
						}
						// handle 2xx cases parsing data received from the device
						if (200 <= statusCode && statusCode < 300) {
							extractExcludeList(exclude);
							populateInformationFromData(stats, responseBody);
						}
					}
				}
			} catch (Exception exc) {
				String errorMessage = exc.getMessage();
				if (StringUtils.isNullOrEmpty(errorMessage) && exc.getCause() != null) {
					errorMessage = exc.getCause().getMessage();
				}
				// handle 3xx cases in case of exception
				if (exc.getCause() instanceof ProtocolException) {
					statusCode = parseTo3XXStatusCode(errorMessage);
					if (statusCode == -1) {
						throw new ResourceNotReachableException(errorMessage);
					}
				} else {
					throw new ResourceNotReachableException(errorMessage, exc);
				}
			}
			uriStatusMessage = generateResponseMessage(statusCode);
			stats.put(WebClientConstant.URI_STATUS, uriStatusMessage);
		} else {
			stats.put(WebClientConstant.URI_STATUS, WebClientConstant.NOT_CONFIGURED);
		}
		extStats.setStatistics(stats);
		return Collections.singletonList(extStats);
	}

	/**
	 * Return a string with status code & its description
	 *
	 * @return String This returns the message as "<status code> <description>"
	 */
	private String generateResponseMessage(int statusCode) {
		return statusCode + WebClientConstant.SPACE + HttpStatus.getDescription(statusCode);
	}

	/**
	 * Extract the content type from httpEntity
	 *
	 * @param httpEntity HttpEntity
	 * @return {@code contentType}
	 */
	private String getContentType(HttpEntity httpEntity) {
		String contentType = WebClientConstant.NO_RESPONSE_CONTENT_TYPE;
		Header header = httpEntity.getContentType();
		if (header != null && !StringUtils.isNullOrEmpty(header.getValue())) {
			contentType = header.getValue();
		}
		return contentType;
	}

	/**
	 * Concatenates the {@code baseRequestUrl} with the uri passed in provided its not empty. Will also add the "/" if its not present in the
	 * {@code baseRequestUrl}. <br>
	 * This method has package visibility so other classes in base communicator package can access it if needed.
	 *
	 * @param uri URI to append to a base URL
	 * @return completed {@code baseRequestUrl}
	 */
	private String buildRequestUrl(String uri) {
		if (StringUtils.isNullOrEmpty(uri)) {
			return this.baseRequestUrl;
		} else if (uri.indexOf("://") > 0) {
			return uri;
		} else {
			return this.baseRequestUrl.endsWith("/") ? this.baseRequestUrl + uri : this.baseRequestUrl + "/" + uri;
		}
	}

	/**
	 * Returns a URL string with: {@code protocol} + "://" + {@code host} + ":" + {@code port} . Provided {@code baseUri} is not null/empty and isn't preceded
	 * by a "/" it will append a "/" and {@code baseUri} value to the end of it.
	 */
	private void buildBaseUrl() {
		StringBuilder uriBuilder = new StringBuilder();
		uriBuilder.append(this.getProtocol()).append("://");
		uriBuilder.append(this.getHost());
		uriBuilder.append(':').append(this.getPort());
		if (!StringUtils.isNullOrEmpty(this.getBaseUri())) {
			if (!this.getBaseUri().startsWith("/")) {
				uriBuilder.append('/');
			}
			uriBuilder.append(this.getBaseUri());
		}
		this.baseRequestUrl = uriBuilder.toString();
	}

	/**
	 * Parse error message to 3xx status code
	 * Return -1 if out of range 3xx or in range 3xx but not in HttpStatus map
	 *
	 * @return int This returns the HTTP response status code or -1 if it's out of the valid range
	 */
	private int parseTo3XXStatusCode(String errorMessage) {
		Matcher matcher = WebClientConstant.HTTP_STATUS_CODE_PATTERN.matcher(errorMessage);
		while (matcher.find()) {
			int statusCode = Integer.parseInt(matcher.group(1));
			// in range 3xx
			if (300 <= statusCode && statusCode < 400 && HttpStatus.containsKey(statusCode)) {
				return statusCode;
			}
		}
		return -1;
	}

	/**
	 * Parse information from data after getting data by the request and contribute the data to {@code stats}
	 *
	 * @param stats list statistic of the device
	 * @param data the data is value after getting data by the request
	 */
	private void populateInformationFromData(Map<String, String> stats, String data) {
		if (StringUtils.isNullOrEmpty(data)) {
			throw new ResourceNotReachableException("Error when parsing data, the response is empty");
		}
		try {
			JsonNode deviceInformation = mapper.readTree(data);
			parseInformationByJson(stats, deviceInformation);
		} catch (ResourceNotReachableException e) {
			throw new ResourceNotReachableException(e.getMessage());
		} catch (Exception ex) {
			try {
				data = data.replaceAll(WebClientConstant.LINE_SEPARATOR, "");
				Document doc = documentBuilder.parse(new InputSource(new StringReader(data)));
				NodeList childNodes = doc.getChildNodes();
				if (childNodes.getLength() > 0 && childNodes.item(0).getChildNodes().getLength() > 1) {
					NodeList listElementsByTagName = childNodes.item(0).getChildNodes();
					parseInformationByXml(listElementsByTagName, stats);
				}
			} catch (Exception exc) {
				throw new ResourceNotReachableException("Error when parsing data, the response is not an supported JSON or XML", exc);
			}
		}
	}

	/**
	 * Constructs a new document builder with security features enabled.
	 *
	 * @return a new document builder
	 * @throws ParserConfigurationException thrown if there is a parser configuration exception
	 */
	private DocumentBuilder buildSecureDocumentBuilder() throws ParserConfigurationException {
		final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature(WebClientConstant.DISALLOW_DOCTYPE_DECL, true);
		factory.setFeature(WebClientConstant.EXTERNAL_GENERAL_ENTITIES, false);
		factory.setFeature(WebClientConstant.EXTERNAL_PARAMETER_ENTITIES, false);
		factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
		factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
		return factory.newDocumentBuilder();
	}

	/**
	 * Parse Json data get from the request
	 *
	 * @param stats list statistic of the device
	 * @param data the data is an object to be parsed
	 */
	private void parseInformationByJson(Map<String, String> stats, JsonNode data) {
		Iterator<Entry<String, JsonNode>> fields = data.fields();
		while (fields.hasNext()) {
			Map.Entry<String, JsonNode> field = fields.next();
			if (!isSupportedJsonFormat(field)) {
				continue;
			}
			String jsonName = field.getKey().replace(WebClientConstant.HASH_SIGN, "");
			JsonNode firstLevelValue = field.getValue();
			if (firstLevelValue.isArray()) {
				// array
				contributeJsonArrayValue(stats, null, firstLevelValue, jsonName);
			} else if (firstLevelValue.isObject()) {
				// object
				contributeJsonValue(stats, firstLevelValue, jsonName);
			} else {
				// other
				String value = firstLevelValue.isTextual() ? firstLevelValue.textValue() : firstLevelValue.toString();
				addKeyAndValueIntoStatistics(stats, null, jsonName, value, true);
			}
		}
	}

	/**
	 * Check next step parsing data json
	 *
	 * @param field the field is Map.Entry<String, JsonNode>
	 * @return nextStep this return true if correct rules else return false
	 */
	private boolean isSupportedJsonFormat(Map.Entry<String, JsonNode> field) {
		boolean nextStep = true;
		String jsonName = field.getKey();
		JsonNode jsonValue = field.getValue();
		if (StringUtils.isNullOrEmpty(jsonName) || jsonValue.isNull() || excludedList.contains(jsonName.trim().replace(WebClientConstant.HASH_SIGN, ""))) {
			nextStep = false;
		}
		return nextStep;
	}

	/**
	 * Parsing value form json Array
	 *
	 * @param stats the stats are list statistic of the device
	 * @param parentName the parentName is the parent name of Object
	 * @param value value the value is an object to be parsed
	 * @param key the key is field name in the statistics
	 */
	private void contributeJsonArrayValue(Map<String, String> stats, String parentName, JsonNode value, String key) {
		List<String> listStringData = new ArrayList<>();
		for (int i = 0; i < value.size(); i++) {
			JsonNode jsonNodeItem = value.get(i);
			if (jsonNodeItem.isNull()) {
				continue;
			}
			if (!jsonNodeItem.isObject() && !jsonNodeItem.isArray()) {
				String jsonValue = jsonNodeItem.isTextual() ? jsonNodeItem.textValue() : jsonNodeItem.toString();
				if (!StringUtils.isNullOrEmpty(jsonValue)) {
					listStringData.add(jsonValue);
				}
			}
		}
		if (!listStringData.isEmpty()) {
			// remove the character "[" at first and "]" at the end in array list
			String jsonValue = listStringData.toString().substring(1, listStringData.toString().length() - 1);
			addKeyAndValueIntoStatistics(stats, parentName, key, jsonValue, true);
		}
	}

	/**
	 * Parsing data form json object, it's the second level value of the json
	 * The function only supports parsing data in the form of strings, numbers, text and boolean
	 *
	 * @param stats the stats are list statistic of the device
	 * @param value the value is an object to be parsed
	 * @param parentName the parentName is the parent name of Object
	 */
	private void contributeJsonValue(Map<String, String> stats, JsonNode value, String parentName) {
		Iterator<Entry<String, JsonNode>> fields = value.fields();
		while (fields.hasNext()) {
			Map.Entry<String, JsonNode> field = fields.next();
			String secondLevelKey = field.getKey();
			JsonNode secondLevelValue = field.getValue();
			if (!isSupportedJsonFormat(field)) {
				continue;
			}
			secondLevelKey = secondLevelKey.replace(WebClientConstant.HASH_SIGN, "");
			if (secondLevelValue.isArray()) {
				contributeJsonArrayValue(stats, parentName, secondLevelValue, secondLevelKey);
			} else if (isSupportedJsonFormat(field) && !secondLevelValue.isObject()) {
				String val = secondLevelValue.isTextual() ? secondLevelValue.textValue() : secondLevelValue.toString();
				addKeyAndValueIntoStatistics(stats, parentName, secondLevelKey, val, true);
			}
		}
	}

	/**
	 * Parse XML data get from the request
	 *
	 * @param stats list statistic of the device
	 * @param nodeList the nodeList is an XML list tag that needs to be parsed
	 */
	private void parseInformationByXml(NodeList nodeList, Map<String, String> stats) {
		for (Node nodeItem : iterable(nodeList)) {
			String firstLevelTagName = nodeItem.getNodeName();
			NodeList childNodes = nodeItem.getChildNodes();

			if (excludedList.contains(firstLevelTagName.trim()) || nodeItem.getNodeType() != Node.ELEMENT_NODE) {
				continue;
			}

			if (!isSupportedXMLFormat(nodeList, nodeItem, false)) {
				if (!hasChildElements(nodeItem)) {
					String valueXML = getAndUpdateValueByTagNameXML(stats, "", firstLevelTagName, childNodes.item(0).getNodeValue());
					addKeyAndValueIntoStatistics(stats, "", firstLevelTagName, valueXML, false);
				}
				continue;
			}

			if (hasChildElements(nodeItem)) {
				handleSecondLevelXML(childNodes, stats, firstLevelTagName);
			} else {
				String value = nodeItem.getTextContent();
				addKeyAndValueIntoStatistics(stats, "", firstLevelTagName, value, false);
			}
		}
	}

	/**
	 * Check the XML element contains another XML element or not
	 *
	 * @param node the XML element that needs to check
	 * @return true/false true: hasChild, false: noChild
	 */
	private boolean hasChildElements(Node node) {
		NodeList children = node.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Lambda expression (for creating a new Iterable) and default method to use for-each in {@code NodeList}
	 *
	 * @param nodeList the nodeList is an XML list tag that needs to be parsed
	 * @return Iterable<Node>
	 */
	private Iterable<Node> iterable(final NodeList nodeList) {
		return () -> new Iterator<Node>() {
			private int index = 0;

			@Override
			public boolean hasNext() {
				return index < nodeList.getLength();
			}

			@Override
			public Node next() {
				if (!hasNext()) {
					throw new NoSuchElementException();
				}
				return nodeList.item(index++);
			}
		};
	}

	/**
	 * check next step parsing data xml
	 *
	 * @param nodeList the nodeList is an XML list tag that needs to be parsed
	 * @param nodeItem the nodeItem is child current of nodeList
	 * @param isSecondLevel the isSecondLevel is boolean data if XML has child element is isSecondLevel is true
	 * @return nextStep this return true if correct rules else return false
	 */
	private boolean isSupportedXMLFormat(NodeList nodeList, Node nodeItem, boolean isSecondLevel) {
		List<String> unsupportedTagNameList = getUnsupportedTagNameList(nodeList, isSecondLevel);
		return !unsupportedTagNameList.contains(nodeItem.getNodeName());
	}

	/**
	 * Parsing the data by tag name element of the xml data
	 *
	 * @param nodeList the nodeList is an XML list tag that needs to be parsed
	 * @param stats the stats are list statistic of the device
	 * @param parentName the parentName is tag Name of the parent element
	 */
	private void handleSecondLevelXML(NodeList nodeList, Map<String, String> stats, String parentName) {
		for (Node nodeItem : iterable(nodeList)) {
			String secondLevelTagName = nodeItem.getNodeName();

			if (excludedList.contains(secondLevelTagName.trim()) || nodeItem.getNodeType() != Node.ELEMENT_NODE) {
				continue;
			}

			if (!isSupportedXMLFormat(nodeList, nodeItem, true)) {
				String value = nodeItem.getChildNodes().item(0).getNodeValue();
				if (nodeItem.getChildNodes().getLength() == 1 && !StringUtils.isNullOrEmpty(value)) {
					String valueXML = getAndUpdateValueByTagNameXML(stats, parentName, secondLevelTagName, nodeItem.getChildNodes().item(0).getNodeValue());
					addKeyAndValueIntoStatistics(stats, parentName, secondLevelTagName, valueXML, false);
				}
				continue;
			}
			if (nodeItem.getChildNodes().getLength() == 1) {
				String xmlValue = getAndUpdateValueByTagNameXML(stats, parentName, secondLevelTagName, nodeItem.getTextContent());
				addKeyAndValueIntoStatistics(stats, parentName, secondLevelTagName, xmlValue, false);
			}

		}
	}

	/**
	 * Add key and value into statistics
	 * if parentName != empty then data put into statistics: parentName#key : value
	 * if parentName == empty then data put into statistics: key : value
	 *
	 * @param stats the stats are list statistic of the device
	 * @param parentName the name is parent name of object
	 * @param key the key is field String first in the map<String,String>
	 * @param value the value is field String second in the map<String,String>
	 * @param jsonContent the jsonContent is boolean type if the jsonContent is true then parsing JSON content else jsonContent is false the parsing XML content
	 */
	private void addKeyAndValueIntoStatistics(Map<String, String> stats, String parentName, String key, String value, boolean jsonContent) {
		if (!StringUtils.isNullOrEmpty(key) && !StringUtils.isNullOrEmpty(value)) {
			//check for duplicate key JSON at level one and two
			if (jsonContent && ((StringUtils.isNullOrEmpty(parentName) && stats.containsKey(key)) || (!StringUtils.isNullOrEmpty(parentName) && stats.containsKey(
					parentName + WebClientConstant.HASH_SIGN + key)))) {
				throw new ResourceNotReachableException("Error when parsing data,the JSON key is duplicate: " + key);
			}
			if (StringUtils.isNullOrEmpty(parentName)) {
				stats.put(key, value);
			} else {
				stats.put(parentName + WebClientConstant.HASH_SIGN + key, value);
			}
		}
	}

	/**
	 * Add the value into xmlValue
	 *
	 * @param stats the stats are list statistic of the device
	 * @param parentName the name is parent name of object
	 * @param key the key is field String first in the map<String,String>
	 * @param value the value is field String second in the map<String,String>
	 * @return String the String is value in  map<String,String>
	 */
	private String getAndUpdateValueByTagNameXML(Map<String, String> stats, String parentName, String key, String value) {
		String xmlValue = value;
		String xmlKey;
		if (StringUtils.isNullOrEmpty(parentName)) {
			xmlKey = key.trim();
		} else {
			xmlKey = parentName.trim() + WebClientConstant.HASH_SIGN + key.trim();
		}
		if (stats.containsKey(xmlKey)) {
			xmlValue = stats.get(xmlKey) + WebClientConstant.COMMA + WebClientConstant.SPACE + value;
		}
		return xmlValue;
	}

	/**
	 * Check element tag name of XML if correct rules returns list tagName no support parse XML
	 *
	 * @param nodeList the nodeList is an XML list tag that needs to be parsed
	 * @param isSecondLevel the isSecondLevel is boolean if XML has child element is isSecondLevel is true
	 * @return List<String> the List<String> is list name of tag XML
	 */
	private List<String> getUnsupportedTagNameList(NodeList nodeList, boolean isSecondLevel) {
		Map<String, Boolean> tagNameToIsDuplicated = new HashMap<>();
		List<String> listUnsupportedTagName = new ArrayList<>();

		for (Node nodeItem : iterable(nodeList)) {
			String tagName = nodeItem.getNodeName();
			boolean isExists = tagNameToIsDuplicated.containsKey(tagName);
			tagNameToIsDuplicated.put(tagName, isExists);
			if (isSecondLevel && hasChildElements(nodeItem)) {
				listUnsupportedTagName.add(tagName);
			}
		}
		for (Map.Entry<String, Boolean> itemTagName : tagNameToIsDuplicated.entrySet()) {
			if (Boolean.TRUE.equals(itemTagName.getValue())) {
				listUnsupportedTagName.add(itemTagName.getKey());
			}
		}
		return listUnsupportedTagName;
	}

	/**
	 * Convert the parseContent string to boolean
	 *
	 * @return boolean the boolean is true or false
	 */
	private boolean isSupportParseContent() {
		if (parseContent == null) {
			// if the parseContent is not configured, do not parse the response
			return false;
		}
		if (parseContent.isEmpty()) {
			throw new IllegalArgumentException("The parseContent can't be empty. Please update the parseContent to true or false");
		}
		String isParseContentData = parseContent.toLowerCase();
		if (WebClientConstant.TRUE.equals(isParseContentData)) {
			return true;
		}
		if (WebClientConstant.FALSE.equals(isParseContentData)) {
			return false;
		}
		throw new IllegalArgumentException("The parseContent has a boolean data type (true or false). Please update parseContent: " + parseContent);
	}

	/**
	 * Check content type is valid
	 *
	 * @param contentType the contentType is content-type of response
	 * @return boolean the return boolean true or false
	 */
	private boolean isValidContentType(String contentType) {
		if (contentType.equals(WebClientConstant.NO_RESPONSE_CONTENT_TYPE)) {
			// if the content type is not in the response, do not check it
			return true;
		}
		if (contentType.contains(WebClientConstant.COMMA)) {
			String[] contentTypeList = contentType.split(WebClientConstant.COMMA);
			for (String contentTypeItem : contentTypeList) {
				if (!isSupportContentTypeFormat(contentTypeItem.trim())) {
					return false;
				}
			}
			return true;
		}
		return isSupportContentTypeFormat(contentType);
	}

	/**
	 * Check the content type is supported
	 *
	 * @param contentType the contentType is content-type of response
	 */
	private boolean isSupportContentTypeFormat(String contentType) {
		boolean isSupportContentType = true;
		if (contentType.contains(WebClientConstant.IMAGE) || contentType.contains(WebClientConstant.VIDEO) || contentType.contains(WebClientConstant.AUDIO)) {
			isSupportContentType = false;
		}
		return isSupportContentType;
	}

	/**
	 * Get exclude list from the exclude string
	 *
	 * @param exclude the exclude is the keyword list will be removed from the statistics list
	 */
	private void extractExcludeList(String exclude) {
		if (!StringUtils.isNullOrEmpty(exclude)) {
			String[] excludeListString = exclude.split(WebClientConstant.COMMA);
			for (String excludeEl : excludeListString) {
				excludedList.add(excludeEl.trim().replace(WebClientConstant.HASH_SIGN, ""));
			}
		}
	}
}