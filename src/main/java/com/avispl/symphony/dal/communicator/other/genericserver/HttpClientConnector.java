/*
 * Copyright (c) 2015-2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import java.net.ProxySelector;
import java.security.KeyStore;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Map;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.BasicCookieStore;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.LaxRedirectStrategy;
import org.apache.http.impl.conn.SystemDefaultRoutePlanner;
import org.apache.http.ssl.SSLContextBuilder;
import org.apache.http.ssl.SSLContexts;

import com.avispl.symphony.dal.util.StringUtils;

/**
 * Replicate {@link com.avispl.symphony.dal.communicator.HttpCommunicator} but only handle doGet without exception handling.
 * No authentication is required.
 */
public class HttpClientConnector {
	private String baseUri;
	private String protocol = "http";
	private int port;
	private int timeout = 30000;
	private int maxConnectionsPerRoute;
	private int maxConnectionsTotal;
	private String baseRequestUrl;
	private boolean trustAllCertificates;
	private boolean followPostRedirects;
	private String host;
	private CloseableHttpClient httpClient;
	private HttpClientContext httpClientContext;

	/**
	 * Adds extra headers to the request
	 *
	 * @param request http request to add headers to
	 * @param headers map with headers to add
	 */
	private static void addCustomHeaders(HttpRequestBase request, Map<String, String> headers) {
		if (headers != null) {
			headers.entrySet().forEach((e) -> {
				request.setHeader((String) e.getKey(), (String) e.getValue());
			});
		}
	}

	public HttpClientConnector() {

	}

	public void setFollowPostRedirects(boolean followPostRedirects) {
		this.followPostRedirects = followPostRedirects;
	}

	public void setBaseUri(String baseUri) {
		this.baseUri = baseUri;
	}

	public void setMaxConnectionsPerRoute(int maxConnectionsPerRoute) {
		this.maxConnectionsPerRoute = maxConnectionsPerRoute;
	}

	public void setMaxConnectionsTotal(int maxConnectionsTotal) {
		this.maxConnectionsTotal = maxConnectionsTotal;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}


	public void setTimeout(int timeout) {
		this.timeout = timeout;
	}

	public void setTrustAllCertificates(boolean trustAllCertificates) {

		this.trustAllCertificates = trustAllCertificates;

	}

	public void setHost(String host) {
		this.host = host;
	}

	protected int doGet(String uri) throws Exception {
		return this.doGet(uri, (Map) null);
	}

	/**
	 * This doGet method return status code
	 *
	 * @return int This returns status code
	 */
	protected int doGet(String uri, Map<String, String> headers) throws Exception {
		buildBaseUrl(this.host);
		HttpClient client = this.obtainHttpClient(false);
		String getUri = this.buildRequestUrl(uri);

		HttpGet request = new HttpGet(getUri);
		addCustomHeaders(request, headers);
		HttpResponse response = null;
		try {
			response = client.execute(request, this.httpClientContext);
		} finally {
			if (response instanceof CloseableHttpResponse) {
				((CloseableHttpResponse) response).close();
			}
		}
		return response.getStatusLine().getStatusCode();
	}

	/**
	 * Returns a URL string with: {@code protocol} + "://" + {@code host} + ":" + {@code port} . Provided {@code baseUri} is not null/empty and isn't preceded
	 * by a "/" it will append a "/" and {@code baseUri} value to the end of it.
	 */
	public void buildBaseUrl(String host) throws Exception {
		StringBuilder uriBuilder = new StringBuilder();
		uriBuilder.append(this.protocol).append("://");
		uriBuilder.append(host);
		uriBuilder.append(':').append(this.port);
		if (!StringUtils.isNullOrEmpty(this.baseUri)) {
			if (!this.baseUri.startsWith("/")) {
				uriBuilder.append('/');
			}
			uriBuilder.append(this.baseUri);
		}
		this.baseRequestUrl = uriBuilder.toString();
	}

	/**
	 * Configures {@link HttpClientBuilder}
	 *
	 * @return instance of {@link HttpClientBuilder}
	 * @throws Exception if any error occurs
	 */
	private HttpClientBuilder configureHttpClientBuilder() throws Exception {
		RequestConfig defaultRequestConfig = this.setupRequestConfigParameter();
		HttpClientBuilder httpClientBuilder = HttpClients.custom();
		httpClientBuilder.setDefaultRequestConfig(defaultRequestConfig);
		if (this.maxConnectionsPerRoute > 0) {
			httpClientBuilder.setMaxConnPerRoute(this.maxConnectionsPerRoute);
		}

		if (this.maxConnectionsTotal > 0) {
			httpClientBuilder.setMaxConnTotal(this.maxConnectionsTotal);
		}

		if (this.trustAllCertificates && "https".equalsIgnoreCase(this.protocol)) {
			SSLContextBuilder sslContextBuilder = SSLContexts.custom();
			sslContextBuilder.loadTrustMaterial((KeyStore) null, new TrustStrategy() {
				public boolean isTrusted(X509Certificate[] chain, String authType) throws CertificateException {
					return true;
				}
			});
			SSLContext sslContext = sslContextBuilder.build();
			httpClientBuilder.setSSLContext(sslContext);
			HostnameVerifier hostNameVerifier = new HostnameVerifier() {
				public boolean verify(String s, SSLSession sslSession) {
					return true;
				}
			};
			httpClientBuilder.setSSLHostnameVerifier(hostNameVerifier);
		}
		httpClientBuilder.setRoutePlanner(new SystemDefaultRoutePlanner(ProxySelector.getDefault()));
		if (this.followPostRedirects) {
			httpClientBuilder.setRedirectStrategy(new LaxRedirectStrategy());
		}

		httpClientBuilder.setDefaultCookieStore(new BasicCookieStore());
		return httpClientBuilder;
	}

	private RequestConfig setupRequestConfigParameter() {
		return RequestConfig.custom().setSocketTimeout(this.timeout).setConnectTimeout(this.timeout).setConnectionRequestTimeout(this.timeout).setCookieSpec("default").build();
	}

	/**
	 * Concatenates the {@code baseRequestUrl} with the uri passed in provided its not empty. Will also add the "/" if its not present in the
	 * {@code baseRequestUrl}. <br>
	 * This method has package visibility so other classes in base communicator package can access it if needed.
	 *
	 * @param uri URI to append to a base URL
	 * @return completed {@code baseRequestUrl}
	 */
	String buildRequestUrl(String uri) {
		if (StringUtils.isNullOrEmpty(uri)) {
			return this.baseRequestUrl;
		} else if (uri.indexOf("://") > 0) {
			return uri;
		} else {
			return this.baseRequestUrl.endsWith("/") ? this.baseRequestUrl + uri : this.baseRequestUrl + "/" + uri;
		}
	}

	/**
	 * HttpCommunicator createClient() will populate the {@code baseRequestUrl} by calling {@code buildBaseUrl()}. If our {@code httpClient} is not initialized
	 * we configure it via the {@code configureHttpClient()} and populate our {@code httpClient} credentials with {@code login} / {@code password} unless the
	 * {@code login} is null/empty. Depending on parameter passed in we then call {@code authenticate()} and update {@code timestamp} member. <br>
	 * This method has package visibility so other classes in base communicator package can access it if needed.
	 *
	 * @param shouldAuthenticate boolean value to indicate whether we should call {@code authenticate()} method or not
	 * @return http client
	 * @throws IllegalStateException when the {@code host} member is empty
	 * @throws Exception if any other error occurs
	 */
	HttpClient obtainHttpClient(boolean shouldAuthenticate) throws Exception {
		if (!shouldAuthenticate) {
			if (this.httpClient != null) {
				CloseableHttpClient var3 = this.httpClient;
				return var3;
			}
		}
		CloseableHttpClient client;
		if (this.httpClient == null) {
			HttpClientBuilder httpClientBuilder = this.configureHttpClientBuilder();
			client = this.httpClient = httpClientBuilder.build();
		} else {
			client = this.httpClient;
		}
		return client;
	}
}
