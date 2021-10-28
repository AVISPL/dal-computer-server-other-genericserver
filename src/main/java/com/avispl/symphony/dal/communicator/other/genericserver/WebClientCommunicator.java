/*
 * Copyright (c) 2015-2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.api.dal.monitor.Monitorable;
import com.avispl.symphony.dal.communicator.RestCommunicator;
import com.avispl.symphony.dal.communicator.other.genericserver.utils.HttpStatus;
import com.avispl.symphony.dal.util.StringUtils;

/**
 * This class checks accessible to a given URI then return the HTTP status code to Symphony
 */
public class WebClientCommunicator extends RestCommunicator implements Monitorable {

	private static final String WHITE_SPACE = " ";
	private static final String URI_STATUS = "URI Status";
	private static final String NOT_CONFIGURED = "Not Configured";
	private static final Pattern HTTP_STATUS_CODE_PATTERN = Pattern.compile("(\\d{3})");
	/**
	 * URI string that is used to check accessible.
	 */
	private String URI;
	private String baseRequestUrl;

	/**
	 * Constructor
	 */
	public WebClientCommunicator() {
		// WebClientCommunicator no-args constructor
	}

	/**
	 * Retrieves {@code URI}
	 *
	 * @return String This returns the current URI
	 */
	public String getURI() {
		return this.URI;
	}

	/**
	 * Sets {@code URI}
	 *
	 * @param URI This is the URI to be set
	 */
	public void setURI(String URI) {
		this.URI = URI;
	}

	/**
	 * This method is called by Symphony to get the list of statistics to be displayed
	 * {@inheritDoc}
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
				statusCode = Integer.parseInt(doGet(this.URI));
				if (!HttpStatus.containsKey(statusCode)) {
					throw new ResourceNotReachableException("Response status code not in range");
				}
			} catch (Exception exc) {
				String errorMessage = exc.getMessage();
				if (StringUtils.isNullOrEmpty(errorMessage)) {
					if (exc.getCause() != null) {
						errorMessage = exc.getCause().getMessage();
					}
				}
				statusCode = parseToStatusCode(errorMessage);
				if (statusCode == -1) {
					throw new ResourceNotReachableException(errorMessage);
				}
			}
			uriStatusMessage = generateResponseMessage(statusCode);
			stats.put(URI_STATUS, uriStatusMessage);
		} else {
			stats.put(URI_STATUS, NOT_CONFIGURED);
		}
		extStats.setStatistics(stats);
		return Collections.singletonList(extStats);
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
		return statusCode + WHITE_SPACE + HttpStatus.getDescription(statusCode);
	}

	/**
	 * Return the status code.
	 *
	 * {@inheritDoc}
	 *
	 * @return This returns the status code.
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

		try {
			response = client.execute(request);
		} finally {
			if (response instanceof CloseableHttpResponse) {
				((CloseableHttpResponse) response).close();
			}
		}
		return String.valueOf(response.getStatusLine().getStatusCode());
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
	private void buildBaseUrl() throws Exception {
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
	 * Parse error message to status code
	 * Return -1 if out of range 200
	 *
	 * @return int This returns the HTTP response status code or -1 if it's out of the valid range
	 */
	private int parseToStatusCode(String errorMessage) {
		Matcher matcher = HTTP_STATUS_CODE_PATTERN.matcher(errorMessage);
		while (matcher.find()) {
			return Integer.parseInt(matcher.group(1));
		}
		return -1;
	}

}