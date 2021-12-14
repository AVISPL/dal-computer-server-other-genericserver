/*
 * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.regex.Matcher;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.ProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.util.EntityUtils;

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
 * @since 1.0.1
 */
public class WebClientCommunicator extends RestCommunicator implements Monitorable {

	/**
	 * URI string that is used to check accessible.
	 */
	private String URI;
	private String exclude;
	private String parseContent;
	private boolean isParseContent;
	private String baseRequestUrl;
	private List<String> excludedList = new ArrayList<>();
	private ObjectMapper mapper = new ObjectMapper();
	final String statusCodeAndDataBody = UUID.randomUUID().toString().replace(WebClientConstant.DASH, "");
	final String dataBodyAndContentType = UUID.randomUUID().toString().replace(WebClientConstant.DASH, "");

	/**
	 * Constructor
	 */
	public WebClientCommunicator() {
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
	 * Sets {@code URI}
	 *
	 * @param URI the {@code java.lang.String} field
	 */
	public void setURI(String URI) {
		this.URI = URI;
	}

	/**
	 * Retrieves {@code {@link #exclude }}
	 *
	 * @return value of {@link #exclude}
	 */
	public String getExclude() {
		return exclude;
	}

	/**
	 * Retrieves {@code {@link #parseContent }}
	 *
	 * @return value of {@link #parseContent}
	 */
	public String getParseContent() {
		return parseContent;
	}

	/**
	 * Sets {@code Content}
	 *
	 * @param parseContent the {@code java.lang.String} field
	 */
	public void setParseContent(String parseContent) {
		this.parseContent = parseContent;
	}

	/**
	 * Sets {@code exclude} and set excludes after getExclude from the configuration properties on the symphony portal
	 *
	 * @param exclude the {@code java.lang.String} field
	 */
	public void setExclude(String exclude) {
		this.exclude = exclude;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method is called by Symphony to get the list of statistics to be displayed
	 *
	 * @return List<Statistics> This returns the list of statistics
	 */
	@Override
	public List<Statistics> getMultipleStatistics() throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Perform doGet() at host %s with port %s", this.host, this.getPort()));
		}
		final ExtendedStatistics extStats = new ExtendedStatistics();
		final Map<String, String> stats = new HashMap<>();
		String uriStatusMessage;
		if (!StringUtils.isNullOrEmpty(this.URI)) {
			int statusCode;
			try {
				isParseContent = isSupportParseContentStringToBoolean();
				String bodyResponse = null;
				String contentType = null;
				String response = doGet(this.URI);
				int lenStatusCode = response.indexOf(statusCodeAndDataBody);
				int lenDataBody = response.indexOf(dataBodyAndContentType);
				if (lenDataBody == -1) {
					statusCode = Integer.parseInt(response);
				} else {
					if (lenStatusCode != -1) {
						statusCode = Integer.parseInt(response.substring(0, lenStatusCode));
					} else {
						statusCode = Integer.parseInt(response.substring(0, lenDataBody));
					}
				}
				if (!HttpStatus.containsKey(statusCode)) {
					throw new ResourceNotReachableException("Response status code not in range");
				}
				if (isParseContent) {
					contentType = response.substring(lenDataBody + dataBodyAndContentType.length());
					if (contentType.equals(WebClientConstant.INVALID) || !isContentTypeValid(contentType)) {
						throw new ResourceNotReachableException("Error the content type invalid");
					}
					if (lenStatusCode != -1) {
						bodyResponse = response.substring(lenStatusCode + statusCodeAndDataBody.length(), lenDataBody);
					}
					// handle 2xx cases parsing data received from the device
					if (200 <= statusCode && statusCode < 300) {
						getExcludeList(exclude);
						populateInformationFromData(stats, bodyResponse);
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
					throw new ResourceNotReachableException(errorMessage);
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
	 * Get list exclude by the string exclude
	 *
	 * @param exclude the exclude is the keyword list will be removed from the statistics list
	 */
	private void getExcludeList(String exclude) {
		if (!StringUtils.isNullOrEmpty(exclude)) {
			List<String> list;
			list = Arrays.asList(exclude.split(WebClientConstant.COMMA));
			for (int i = 0; i < list.size(); i++) {
				excludedList.add(capitalize(list.get(i).trim()).replace(WebClientConstant.NUMBER, ""));
			}
		}
	}

	@Override
	protected void internalInit() throws Exception {
		super.internalInit();
		buildBaseUrl();
	}

	/**
	 * WebClientCommunicator doesn't require authentication
	 *
	 * {@inheritDoc}
	 */
	@Override
	protected void authenticate() {
		// WebClientCommunicator doesn't require authentication
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
		if (this.logger.isTraceEnabled()) {
			this.logger.debug("Performing a GET operation for " + getUri);
		}

		HttpGet request = new HttpGet(getUri);
		HttpResponse response = null;
		String dataBody = null;
		String contentType = WebClientConstant.INVALID;
		try {
			response = client.execute(request);
			HttpEntity httpEntity = response.getEntity();
			if (httpEntity != null && isParseContent) {
				dataBody = EntityUtils.toString(httpEntity);
				Header header = httpEntity.getContentType();
				if (header != null && header.getValue() != null) {
					contentType = header.getValue();
				}
			}
		} finally {
			if (response instanceof CloseableHttpResponse) {
				((CloseableHttpResponse) response).close();
			}
		}
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(response.getStatusLine().getStatusCode());
		if (isParseContent) {
			if (!StringUtils.isNullOrEmpty(dataBody)) {
				stringBuilder.append(statusCodeAndDataBody);
				stringBuilder.append(dataBody);
			}
			stringBuilder.append(dataBodyAndContentType);
			stringBuilder.append(contentType);
		}
		return stringBuilder.toString();
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
	 * Change string data to boolean data
	 *
	 * @return boolean the boolean is true or false
	 */
	private boolean isSupportParseContentStringToBoolean() {
		if (StringUtils.isNullOrEmpty(parseContent)) {
			return false;
		} else {
			String isParseContentData = parseContent.toLowerCase();
			if (!WebClientConstant.TRUE.equals(isParseContentData) && !WebClientConstant.FALSE.equals(isParseContentData)) {
				throw new ResourceNotReachableException("The parseContent has a boolean data type (true or false). Please re-enter parseContent: " + parseContent);
			}
			if (WebClientConstant.FALSE.equals(isParseContentData)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Check content type is valid
	 *
	 * @param contentType the contentType is content-type of response
	 * @return boolean the return boolean true or false
	 */
	private boolean isContentTypeValid(String contentType) {
		List<String> contentTypeList;
		boolean isContentTypeValid = true;
		if (contentType.contains(WebClientConstant.COMMA)) {
			//handle case multi-option content type
			contentTypeList = Arrays.asList(contentType.split(WebClientConstant.COMMA));
			for (String contentTypeItem : contentTypeList) {
				if (!isSupportContentTypeFormat(contentTypeItem.trim())) {
					isContentTypeValid = false;
					break;
				}
			}
		} else if (!isSupportContentTypeFormat(contentType)) {
			isContentTypeValid = false;
		}
		return isContentTypeValid;
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
	 * Parse information from data after getting data by the request
	 *
	 * @param stats list statistic of the device
	 * @param data the data is value after getting data by the request
	 * @return stats This returns the map key and value of statistic
	 */
	private void populateInformationFromData(Map<String, String> stats, String data) {
		try {
			JsonNode deviceInformation = mapper.readTree(data);
			parseInformationByJson(stats, deviceInformation);
		} catch (Exception e) {
			try {
				//parseInformationByXML
				stats.put(WebClientConstant.CONTENT_TYPE, WebClientConstant.XML);
			} catch (Exception exc) {
				throw new ResourceNotReachableException("Error when parsing data");
			}
		}
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
			String jsonName = field.getKey();
			JsonNode jsonValue = field.getValue();
			if (!isSupportedJsonFormat(field)) {
				continue;
			}
			jsonName = jsonName.replace(WebClientConstant.NUMBER, "");
			if (jsonValue.isArray()) {
				contributeJsonArrayValue(stats, "", jsonValue, jsonName);
			} else {
				if (jsonValue.isObject()) {
					contributeJsonValue(stats, jsonValue, jsonName);
				} else {
					String value = jsonValue.isTextual() ? jsonValue.textValue() : jsonValue.toString();
					addKeyAndValueIntoStatistics(stats, "", jsonName, value);
				}
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
		if (StringUtils.isNullOrEmpty(jsonName) || jsonValue.isNull() || excludedList.contains(capitalize(jsonName))) {
			nextStep = false;
		}
		return nextStep;
	}

	/**
	 * Parsing data form json Array
	 *
	 * @param stats the stats are list statistic of the device
	 * @param parentName the parentName is the parent name of Object
	 * @param data data the data is an object to be parsed
	 * @param key the key is field name in the statistics
	 */
	private void contributeJsonArrayValue(Map<String, String> stats, String parentName, JsonNode data, String key) {
		List<String> listStringData = new ArrayList<>();
		for (int i = 0; i < data.size(); i++) {
			JsonNode jsonNodeItem = data.get(i);
			if (!jsonNodeItem.isNull() && !jsonNodeItem.isObject() && !jsonNodeItem.isArray()) {
				String xmlValue = jsonNodeItem.isTextual() ? jsonNodeItem.textValue() : jsonNodeItem.toString();
				if (!StringUtils.isNullOrEmpty(xmlValue)) {
					listStringData.add(xmlValue);
				}
			}
		}
		if (!listStringData.isEmpty()) {
			//remove the character "[" at first and "]" at the end in array list
			String jsonValue = listStringData.toString().substring(1, listStringData.toString().length() - 1);
			addKeyAndValueIntoStatistics(stats, parentName, key, jsonValue);
		}
	}

	/**
	 * Parsing data form json object
	 * The function only supports parsing data in the form of strings, numbers, text and boolean
	 *
	 * @param stats the stats are list statistic of the device
	 * @param data data the data is an object to be parsed
	 * @param parentName the parentName is the parent name of Object
	 */
	private void contributeJsonValue(Map<String, String> stats, JsonNode data, String parentName) {
		Iterator<Entry<String, JsonNode>> fields = data.fields();
		while (fields.hasNext()) {
			Map.Entry<String, JsonNode> field = fields.next();
			String jsonName = field.getKey();
			JsonNode jsonValue = field.getValue();
			if(!isSupportedJsonFormat(field)){
				continue;
			}
			jsonName = jsonName.replace(WebClientConstant.NUMBER, "");
			if (jsonValue.isArray()) {
				contributeJsonArrayValue(stats, parentName, jsonValue, jsonName);
			} else if (isSupportedJsonFormat(field) && !jsonValue.isObject()) {
				String value = jsonValue.isTextual() ? jsonValue.textValue() : jsonValue.toString();
				addKeyAndValueIntoStatistics(stats, parentName, jsonName, value);
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
	 */
	private void addKeyAndValueIntoStatistics(Map<String, String> stats, String parentName, String key, String value) {
		if (!StringUtils.isNullOrEmpty(key) && !StringUtils.isNullOrEmpty(value)) {
			if (StringUtils.isNullOrEmpty(parentName)) {
				stats.put(capitalize(key), value);
			} else {
				stats.put(capitalize(parentName) + WebClientConstant.NUMBER + capitalize(key), value);
			}
		}
	}

	/**
	 * Capitalize first letter of string
	 *
	 * @param str str is String value
	 * @return capitalize String
	 */
	public static String capitalize(String str) {
		if (StringUtils.isNullOrEmpty(str)) {
			return "";
		}
		return str.substring(0, 1).toUpperCase() + str.substring(1);
	}
}