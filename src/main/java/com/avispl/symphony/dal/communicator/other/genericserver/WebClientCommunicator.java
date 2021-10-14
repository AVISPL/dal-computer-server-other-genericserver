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

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.api.dal.monitor.Monitorable;
import com.avispl.symphony.dal.communicator.HttpCommunicator;
import com.avispl.symphony.dal.util.StringUtils;

/**
 * This class checks accessible to a given URI then return the HTTP status code to Symphony
 */
public class WebClientCommunicator extends HttpCommunicator implements Monitorable {
	/**
	 * Map to store HTTP status code and its description.
	 */
	private static final Map<Integer, String> STATUS_CODE_MAP = new HashMap<>();
	private static final Pattern HTTP_STATUS_CODE_PATTERN = Pattern.compile("(\\d{3})");
	private static final String SPLIT_LINE = "\n";
	private static final String URI_STATUS = "URI Status";
	private static final String NOT_CONFIGURED = "Not Configured";
	private static final String EMPTY = "";

	static {
		STATUS_CODE_MAP.put(200, " OK");
		STATUS_CODE_MAP.put(400, " Bad Request");
		STATUS_CODE_MAP.put(403, " Forbidden");
		STATUS_CODE_MAP.put(401, " Unauthorized");
	}

	/**
	 * URI string that is used to check accessible.
	 */
	private String URI;

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
	public List<Statistics> getMultipleStatistics() {
		if (logger.isDebugEnabled()) {
			logger.debug("Perform doGet() at host " + this.getHost());
		}
		final ExtendedStatistics extStats = new ExtendedStatistics();
		final Map<String, String> stats = new HashMap<>();
		String uriStatusMessage;
		if (!StringUtils.isNullOrEmpty(this.URI)) {
			try {
				doGet(this.URI);
				uriStatusMessage = generateResponseMessage(200);
			} catch (Exception e) {
				String errorMessage = StringUtils.isNullOrEmpty(e.getMessage()) ? EMPTY : e.getMessage();
				int statusCode = parseToStatusCode(errorMessage);
				if (!STATUS_CODE_MAP.containsKey(statusCode)) {
					// split the error message to operation + customMessage
					String apiErrorMessage = errorMessage.split(SPLIT_LINE)[0].trim();
					throw new ResourceNotReachableException(apiErrorMessage);
				}
				uriStatusMessage = generateResponseMessage(statusCode);
			}
			stats.put(URI_STATUS, uriStatusMessage);
		} else {
			stats.put(URI_STATUS, NOT_CONFIGURED);
		}
		extStats.setStatistics(stats);
		return Collections.singletonList(extStats);
	}

	/**
	 * WebClientCommunicator doesn't require authentication
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
		return statusCode + STATUS_CODE_MAP.get(statusCode);
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
			int statusCodeInt = Integer.parseInt(matcher.group(1));
			// Only return the 3 digits number in range of HTTP status code
			if (statusCodeInt >= 100 && statusCodeInt < 600) {
				return statusCodeInt;
			}
		}
		return -1;
	}
}